library(plyr)
library(MASS)
library(nnet)
library(randomForest)
library(mda)
library(kernlab)
library(caret)
library(parallel)
library(pROC)
library(boot)
library(reshape2)
library(doParallel)
source('../R/caret_funcs.r')
source('../R/multiclass_roc.r')
source('../R/train_and_test.r')


# start analysis
registerDoParallel(cores = detectCores())

source('../R/supervised_mung.r')
adult$spinks <- LETTERS[adult$spinks]

schemes <- c('sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks')
meth <- c('multinom', 'nnet', 'lda', 'pda', 'rf')
npred <- 25

#results <- list()
#for(ii in seq(length(meth))) {
#  results[[ii]] <- use.model(method = meth[ii],
#                             adult = adult, 
#                             scheme = schemes,
#                             npred = 25)
#}
#names(results) <- meth
#save(results, file = '../data/model_cv_results.rdata')
load('../data/model_cv_results.rdata')

# in sample roc from cv
roc.out <- llply(results, function(oo) 
                 llply(oo$training, function(x) 
                       laply(x, function(y) y$results[c('ROC', 'ROCSD')])))
names(roc.out) <- meth
roc.out <- llply(roc.out, function(x) llply(x, function(y) apply(y, 2, unlist)))
high.roc <- llply(roc.out, function(y) laply(y, function(x) which.max(x[, 1])))


# this is all to make sure i get the most parsimonious within one SE of best
min.grab <- list()
for(ii in seq(length(roc.out))) {
  hold <- list()
  for(jj in seq(length(schemes))) {
    hold[[jj]] <- diff(rev(roc.out[[ii]][[jj]][high.roc[[ii]][jj], ]))
  }
  min.grab[[ii]] <- unlist(hold)
}
names(min.grab) <- meth

roc.subrank <- Map(function(x, y) 
                   Map(function(a, b) a[1:b, 1], x, y), 
                   roc.out, high.roc)
select.roc <- Map(function(x, y) 
                  unlist(Map(function(a, b) max(which(a >= b)) - 1, x, y)), 
                  x = roc.subrank, y = min.grab)
select.roc <- llply(select.roc, function(x) ifelse(x != 0, x, x + 1))



roc.melt <- llply(roc.out, function(l) 
                  Reduce(rbind, Map(function(x, y) 
                                    cbind(npred = seq(from = 3,
                                                      to = nrow(x) + 2), 
                                          scheme = y, x), 
                                    l, schemes)))
roc.melt <- Reduce(rbind, Map(function(x, y) 
                              cbind(model = y, x), roc.melt, meth))

roc.melt <- apply(roc.melt, 2, unlist)
roc.melt <- data.frame(roc.melt)
roc.melt$npred <- as.numeric(as.character(roc.melt$npred))
roc.melt$ROC <- as.numeric(as.character(roc.melt$ROC))
roc.melt$ROCSD <- as.numeric(as.character(roc.melt$ROCSD))

roc.melt$ROCmin <- roc.melt$ROC - roc.melt$ROCSD
roc.melt$ROCmax <- roc.melt$ROC + roc.melt$ROCSD

# add point for "best" and "selected"
high.melt <- Reduce(rbind, Map(function(x, y, z) 
                               cbind(model = x, npred = y + 2, scheme = schemes), 
                               meth, high.roc))
high.melt <- cbind(high.melt, 
                   melt(Map(function(a, b) 
                            unlist(Map(function(x, y) x[y, 1], a, b)), 
                            roc.out, high.roc)))
high.melt$npred <- as.numeric(as.character(high.melt$npred))

# select
selc.melt <- Reduce(rbind, Map(function(x, y) 
                               cbind(model = x, npred = y + 2, scheme = schemes), 
                               meth, select.roc))
selc.melt <- cbind(selc.melt, 
                   melt(Map(function(a, b) 
                            unlist(Map(function(x, y) x[y, 1], a, b)), 
                            roc.out, select.roc)))
selc.melt$npred <- as.numeric(as.character(selc.melt$npred))


roc.plot <- ggplot(roc.melt, aes(x = npred, y = ROC))
roc.plot <- roc.plot + geom_linerange(aes(ymin = ROCmin, ymax = ROCmax))
roc.plot <- roc.plot + geom_line()
roc.plot <- roc.plot + facet_grid(model ~ scheme, switch = 'y')

roc.plot <- roc.plot + geom_point(data = high.melt, 
                                  mapping = aes(x = npred, y = value, 
                                                ymin = NULL, ymax = NULL), 
                                  colour = 'blue')
roc.plot <- roc.plot + geom_point(data = selc.melt, 
                                  mapping = aes(x = npred, y = value, 
                                                ymin = NULL, ymax = NULL), 
                                  colour = 'red')
roc.plot <- roc.plot + labs(x = 'Model complexity', y = 'AUC')
ggsave(plot = roc.plot, filename = '../doc/figure/emys_model_sel.pdf',
       width = 7.5, height = 8)


# out of sample roc from test
# with best model
test.pred <- Map(function(a, b) Map(function(x, y) y[[x]], a, b$testing), 
                 select.roc, results)
 
oos.roc <- Map(function(a, b) 
               unlist(Map(function(x, y, z) allvone(x, y[, z]), 
                          a, b$testing.dataset, schemes)),
               test.pred, results)
oos.melt <- Reduce(rbind, Map(function(x, y) 
                              cbind(model = y, scheme = schemes, value = x), 
                              oos.roc, meth))
oos.melt <- data.frame(oos.melt)
oos.melt$value <- as.numeric(as.character(oos.melt$value))
oos.mean <- ddply(oos.melt, .(scheme), summarize, mean = mean(value))

oos.plot <- ggplot(oos.melt, aes(x = model, y = value))
oos.plot <- oos.plot + geom_point() + facet_grid(. ~ scheme)
oos.plot <- oos.plot + geom_hline(data = oos.mean, 
                                  mapping = aes(yintercept = mean))
oos.plot <- oos.plot + labs(x = 'Model type', y = 'AUC')
oos.plot <- oos.plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(plot = oos.plot, filename = '../doc/figure/emys_oos_sel.pdf',
       width = 7.5, height = 4)
