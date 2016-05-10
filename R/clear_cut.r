library(shapes)
library(MuMIn)
library(geomorph)
library(plyr)
library(stringr)
library(reshape2)
library(cluster)
library(MASS)
library(nnet)
library(randomForest)
library(caret)
library(parallel)
library(doParallel)
library(xtable)
library(boot)
library(pROC)
library(ggplot2)
library(grid)
library(scales)
# source files
source('../R/array2df.r')
source('../R/df2array.r')
source('../R/caret_funcs.r')  # helper functions for train
source('../R/support_functions.r')
source('../R/multiclass_roc.r')
source('../R/train_and_test.r')

turt.out <- list()

# actually start doing analysis...
newturt <- list.files('../data/new_turtle', 
                      pattern = 'adult', 
                      full.names = TRUE)
turt <- llply(newturt, function(x) read.csv(x, header = FALSE))
numbers <- llply(turt, function(x) x[, 1:2])
centroids <- llply(turt, function(x) x[, ncol(x)])
turt <- llply(turt, function(x) x[, -c(1:2, ncol(x))])
# number, museum #, lands...., centroid
turt <- Reduce(rbind, turt)
turt.align <- df2array(turt, n.land = 26, n.dim = 2)
turt.proc <- procGPA(turt.align)
turt.scores <- turt.proc$scores

centroids <- scale(unlist(centroids))
turt.name <- laply(str_split(newturt, '\\/'), function(x) x[length(x)])
turt.name <- str_trim(str_extract(turt.name, '\\s(.*?)\\s'))
turt.out[[1]] <- data.frame(sp = rep(turt.name, 
                                        times = laply(numbers, nrow)), 
                               size = centroids,
                               inter = (centroids * turt.scores[, 1]),
                               turt.scores, stringsAsFactors = FALSE)

trac <- list.files('../data/trach', pattern = 'txt', full.names = TRUE)
turt <- llply(trac, function(x) 
              read.table(x, header = FALSE, stringsAsFactors = FALSE))
# lands...., centroid
centroids <- scale(unlist(llply(turt, function(x) x[, ncol(x)])))
ids <- Reduce(c, Map(function(x, y) 
                     rep(y, times = nrow(x)), 
                     x = turt, y = c('a', 'b')))
turt <- llply(turt, function(x) x[, -(ncol(x))])
turt <- Reduce(rbind, turt)
turt.align <- df2array(turt, n.land = 26, n.dim = 2)
turt.proc <- procGPA(turt.align)
turt.scores <- turt.proc$scores

turt.out[[2]] <- data.frame(sp = ids, size = centroids,
                               inter = (centroids * turt.scores[, 1]),
                               turt.scores, stringsAsFactors = FALSE)


meth <- c('multinom', 'nnet', 'lda', 'pda', 'rf')
schemes <- c('cc7', 'trac')
#results <- list()
#for(ii in seq(length(meth))) {
#  results[[ii]] <- abrv.model(method = meth[ii], 
#                              adult = turt.out, 
#                              scheme = schemes, 
#                              npred = 25)
#}
#names(results) <- meth
#save(results, file = '../data/others_cv_results.rdata')
load('../data/others_cv_results.rdata')


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

# map values of schemes to useful names
# map values of models to correct abbreviation
roc.melt$model <- mapvalues(roc.melt$model, from = unique(roc.melt$model),
                            to = c('MLR', 'NN', 'LDA', 'PDA', 'RF'))
high.melt$model <- mapvalues(high.melt$model, from = unique(high.melt$model),
                            to = c('MLR', 'NN', 'LDA', 'PDA', 'RF'))
selc.melt$model <- mapvalues(selc.melt$model, from = unique(selc.melt$model),
                            to = c('MLR', 'NN', 'LDA', 'PDA', 'RF'))


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
roc.plot <- roc.plot + labs(x = 'Model complexity (# predictors)', y = 'AUC')
ggsave(plot = roc.plot, filename = '../doc/figure/other_model_sel.pdf',
       width = 4, height = 4)


# out of sample roc from test
# with best model
test.pred <- Map(function(a, b) Map(function(x, y) y[[x]], a, b$testing), 
                 select.roc, results)
 
oos.roc <- Map(function(a, b) 
               unlist(Map(function(x, y) allvone(x, y[, 1]), 
                          a, b$testing.dataset)),
               test.pred, results)
oos.melt <- Reduce(rbind, Map(function(x, y) 
                              cbind(model = y, scheme = schemes, value = x), 
                              oos.roc, meth))
oos.melt <- data.frame(oos.melt)
oos.melt$value <- as.numeric(as.character(oos.melt$value))
oos.mean <- ddply(oos.melt, .(scheme), summarize, mean = mean(value))

# map values of schemes to useful names
# map values of models to correct abbreviation
oos.melt$model <- mapvalues(oos.melt$model, from = unique(oos.melt$model),
                            to = c('MLR', 'NN', 'LDA', 'PDA', 'RF'))

oos.plot <- ggplot(oos.melt, aes(x = model, y = value))
oos.plot <- oos.plot + geom_point() + facet_grid(. ~ scheme)
oos.plot <- oos.plot + geom_hline(data = oos.mean, 
                                  mapping = aes(yintercept = mean))
oos.plot <- oos.plot + labs(x = 'Model type', y = 'AUC')
oos.plot <- oos.plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(plot = oos.plot, filename = '../doc/figure/other_oos_sel.pdf',
       width = 4, height = 3)
