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
# source files
source('../R/array2df.r')
source('../R/df2array.r')
source('../R/shape_distance.r')
source('../R/caret_funcs.r')  # helper functions for train
source('../R/support_functions.r')
source('../R/multi_funcs.r')
source('../R/multiclass_roc.r')

newturt <- list.files('../data/new_turtle', full.names = TRUE)
turt <- llply(newturt, function(x) read.csv(x, header = FALSE))
centroids <- llply(turt, function(x) x[, ncol(x)])
numbers <- llply(turt, function(x) x[, 1:2])
turt <- llply(turt, function(x) x[, -c(1:2, ncol(x))])
# number, museum #, lands...., centroid
turt <- Reduce(rbind, turt)
turt.align <- df2array(turt, n.land = 26, n.dim = 2)
turt.proc <- procGPA(turt.align)
turt.scores <- turt.proc$scores

turt.name <- laply(str_split(newturt, '\\/'), function(x) x[length(x)])
turt.name <- str_trim(str_extract(turt.name, '\\s(.*?)\\s'))
turt.scores <- data.frame(name = rep(turt.name, times = laply(numbers, nrow)), 
                          turt.scores)

# clustering/unsupervised learning
turt.align <- df2array(turt.scores[, -1], n.land = 26, n.dim = 2)
turt.dist <- riem.matrix(turt.align)
tpam <- function(x, k) pam(as.dist(x), k)
#turt.cluster <- clusGap(turt.dist, FUNcluster = tpam, K.max = 10, B = 100)

# supervised learning
max.ad <- nrow(turt.scores) / 50
part <- createDataPartition(turt.scores[, 1], p = 0.75)[[1]]
train <- turt.scores[part, ]
test <- turt.scores[-part, ]

# go over all theoretical fits based on max pred
fort.fit <- rfe(x = train[, seq(from = 2, to = max.ad + 1)], 
                 y = train[, 1], 
                 rfeControl = rf.ctrl, ntree = 100, metric = 'ROC', 
                 sizes = 1:max.ad)
lda.fit <- list()
for(ii in seq(from = 2, to = max.ad)) {
  lda.fit[[ii - 1]] <- lda(train[, seq(from = 2, to = ii + 1)], train[, 1])
}

mnom.fit <- list()
for(ii in seq(from = 2, to = max.ad)) {
  mnom.fit[[ii - 1]] <- multinom(train[, 1] ~ 
                                 as.matrix(train[, seq(from = 2, to = ii + 1)]))
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
                      newdata = train[, seq(from = 2, to = ii + 2)])
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
# TODO out of sample AUC for best model


# process multinomial
# in sample auc for chosing best model
mnom.auc <- c()
for(ii in seq(length(mnom.fit))) {
  mnom.pred.c <- predict(mnom.fit[[ii]])
  mnom.pred.p <- predict(mnom.fit[[ii]], type = 'probs')
  pred <- mnom.pred.c
  obs <- train[, 1]
  prob <- lapply(unique(train[, 1]), function(cl) {
                 pp <- ifelse(pred == cl, 1, 0)
                 oo <- ifelse(obs == cl, 1, 0)
                 prob <- mnom.pred.p[, as.character(cl)]

                 ps <- Metrics::auc(oo, prob)
                 ps})
  mnom.auc[ii] <- colMeans(do.call(rbind, prob))
}
# TODO out of sample AUC for best model
