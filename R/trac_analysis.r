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
source('../R/shape_distance.r')
source('../R/caret_funcs.r')  # helper functions for train
source('../R/support_functions.r')
source('../R/multi_funcs.r')
source('../R/multiclass_roc.r')

# plot settings
theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')
theme_update(axis.text = element_text(size = 20),
             axis.title = element_text(size = 30),
             legend.text = element_text(size = 25),
             legend.title = element_text(size = 26),
             legend.key.size = unit(2, 'cm'),
             strip.text = element_text(size = 20))

# actually start doing analysis
trac <- list.files('../data/trach', pattern = 'txt', full.names = TRUE)
turt <- llply(trac, function(x) 
              read.table(x, header = FALSE, stringsAsFactors = FALSE))
# lands...., centroid
centroids <- unlist(llply(turt, function(x) x[, ncol(x)]))
ids <- Reduce(c, Map(function(x, y) 
                     rep(y, times = nrow(x)), 
                     x = turt, y = c('a', 'b')))
turt <- llply(turt, function(x) x[, -(ncol(x))])
turt <- Reduce(rbind, turt)
turt.align <- df2array(turt, n.land = 26, n.dim = 2)
turt.proc <- procGPA(turt.align)
turt.scores <- turt.proc$scores

turt.scores <- data.frame(ids, centroids, turt.scores)


# supervised learning
max.ad <- nrow(turt.scores) / 10
max.ad <- min(c(max.ad, 26 - 3))
part <- createDataPartition(turt.scores[, 1], p = 0.75)[[1]]
train <- turt.scores[part, ]
test <- turt.scores[-part, ]

# go over all theoretical fits based on max pred
fort.fit <- rfe(x = train[, seq(from = 2, to = max.ad + 2)], 
                 y = train[, 1], 
                 rfeControl = rf.ctrl, ntree = 100, metric = 'ROC', 
                 sizes = 1:max.ad)
lda.fit <- list()
for(ii in seq(from = 2, to = max.ad)) {
  lda.fit[[ii - 1]] <- lda(train[, seq(from = 2, to = ii + 2)], train[, 1])
}

mnom.fit <- list()
for(ii in seq(from = 2, to = max.ad)) {
  dat <- as.matrix(train[, seq(from = 2, to = ii + 2)])
  colnames(dat) <- NULL
  mnom.fit[[ii - 1]] <- multinom(train[, 1] ~ dat)
}

# analysis of the supervised fits
# process rf
# in sample AUC for chosing best model
fort.auc <- fort.fit$results$ROC
# out of sample
fort.pred <- predict(fort.fit, test[, -1])
pred <- fort.pred[, 1]
obs <- test[, 1]
prob <- lapply(unique(test[, 1]), function(cl) {
               pp <- ifelse(pred == cl, 1, 0)
               oo <- ifelse(obs == cl, 1, 0)
               prob <- fort.pred[, as.character(cl)]

               ps <- Metrics::auc(oo, prob)
               ps})
rf.oo.auc <- colMeans(do.call(rbind, prob))

# process lda
# in sample AUC for choosing best model
lda.auc <- c()
for(ii in seq(length(lda.fit))) {
  lda.pred <- predict(lda.fit[[ii]], 
                      newdata = train[, seq(from = 2, to = ii + 3)])
  pred <- lda.pred$class
  obs <- train[, 1]
  prob <- lapply(unique(train[, 1]), function(cl) {
                 pp <- ifelse(pred == cl, 1, 0)
                 oo <- ifelse(obs == cl, 1, 0)
                 prob <- lda.pred$posterior[, as.character(cl)]

                 ps <- Metrics::auc(oo, prob)
                 ps})
  lda.auc[ii] <- colMeans(do.call(rbind, prob))
}
lda.best <- lda.fit[[which.max(lda.auc)]]
lda.bp <- predict(lda.best, 
                  newdata = test[, seq(from = 2, to = which.max(lda.auc) + 3)])
pred <- lda.bp$class
obs <- test[, 1]
prob <- lapply(unique(test[, 1]), function(cl) {
               pp <- ifelse(pred == cl, 1, 0)
               oo <- ifelse(obs == cl, 1, 0)
               prob <- lda.bp$posterior[, as.character(cl)]

               ps <- Metrics::auc(oo, prob)
               ps})
lda.oo.auc <- colMeans(do.call(rbind, prob))
# TODO out of sample AUC for best model


# process multinomial
# in sample auc for chosing best model
mnom.auc <- c()
for(ii in seq(length(mnom.fit))) {
  mnom.pred.c <- predict(mnom.fit[[ii]])
  mnom.pred.p <- predict(mnom.fit[[ii]], type = 'probs')
  pred <- mnom.pred.c
  obs <- train[, 1]
  oo <- ifelse(obs == 'b', 1, 0)
  mnom.auc[[ii]] <- auc(oo, mnom.pred.p)
}
mnom.best <- mnom.fit[[which.max(mnom.auc)]]

dat <- as.matrix(test[, seq(from = 2, to = which.max(mnom.auc) + 3)])
colnames(dat) <- NULL
mnom.oo.c <- predict(mnom.best, dat)
mnom.oo.p <- predict(mnom.best, dat, 'probs')
pred <- mnom.oo.c
obs <- test[, 1]
oo <- ifelse(obs == 'b', 1, 0)
mnom.oo.auc <- auc(oo, mnom.oo.p)


# make some results tables
results <- data.frame(npred = c(which.max(fort.auc), 
                                which.max(lda.auc), 
                                which.max(mnom.auc)), 
                      ii.auc = c(fort.auc[which.max(fort.auc)], 
                                 lda.auc[which.max(lda.auc)], 
                                 mnom.auc[which.max(mnom.auc)]), 
                      oo.auc = c(rf.oo.auc, lda.oo.auc, mnom.oo.auc))
rownames(results) <- c('RF', 'LDA', 'MnL')
res.tab <- xtable(results, label = 'tab:second_res', digits = 3)
print.xtable(res.tab, file = '../doc/trach_tab.tex')


# boot strap the generalization
boot.roc <- function(data, indicies) {
  data <- data[indicies, ]
  pp <- data[, seq(ncol(data) - 1)]
  names(pp) <- gsub('X', '', names(pp))
  tt <- data[, ncol(data)]
  return(allvone(pp, tt))
}

# rf
tester <- cbind(fort.pred, test[, 1])
rf.boot <- boot(data = tester, statistic = boot.roc, R = 1000)
# mnl
tester <- cbind(mnom.oo.c, data.frame(mnom.oo.p), test[, 1])

boot.auc <- function(data, indicies) {
  data <- data[indicies, ]
  oo <- ifelse(data[, ncol(data)] == 'b', 1, 0)
  ps <- auc(oo, data[, 2])
  return(ps)
}
mnl.boot <- boot(data = tester, statistic = boot.auc, R = 1000)

# lda
tester <- cbind(lda.bp$class, data.frame(lda.bp$posterior), test[, 1])
lda.boot <- boot(data = tester, statistic = boot.roc, R = 1000)


# make some output graphs
test.gen <- melt(data.frame(RF = rf.boot$t, MLR = mnl.boot$t, LDA = lda.boot$t))
gen.gg <- ggplot(test.gen, aes(x = value))
gen.gg <- gen.gg + geom_histogram(position = 'identity', binwidth = 1/50)
gen.gg <- gen.gg + labs(x = 'AUC', y = 'Count')
gen.gg <- gen.gg + facet_grid(variable ~ .)
ggsave(file = '../doc/figure/trach_boot.png', plot = gen.gg, 
       width = 15, height = 10)

pc.gg <- ggplot(turt.scores, aes(x = PC1, y = PC2, colour = ids))
pc.gg <- pc.gg + geom_point()
pc.gg <- pc.gg + labs(x = paste0('PC 1 ', turt.proc$percent[1]),
                      y = paste0('PC 2 ', turt.proc$percent[2]))
ggsave(file = '../doc/figure/trach_plot.png', plot = pc.gg, 
       width = 15, height = 10)
