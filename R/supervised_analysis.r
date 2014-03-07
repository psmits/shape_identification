# fit supervised learning models
#
# peter d smits
# psmits@uchicago.edu
###############################################################################

library(MASS)
library(randomForest)
library(caret)
library(parallel)
library(doParallel)

source('../R/caret_funcs.r')  # helper functions for train
source('../R/supervised_mung.r')  # training and testing data

RNGkind(kind = "L'Ecuyer-CMRG")
set.seed(1)

registerDoParallel(cores = detectCores())

# linear discriminate 
set.seed(1)
tlda.a <- Map(function(x, g) {
              out <- list()
              for(ii in seq(max.ad)) {
                out[[ii]] <- train(x[seq(ii)], x[, g], method = 'lda',
                                   metric = 'ROC', trControl = ctrl)
              }
              out},
              x = adult.train, g = groups)


# multinomial logistic regression
set.seed(1)
tmulti.a <- mapply(multi.train,
                   form = tform.a, data = adult.train,
                   MoreArgs = list(method = 'multinom'
                                   , metric = 'ROC'
                                   , trControl = ctrl
                                   , maxit = 1000),
                   SIMPLIFY = FALSE)


# random forests
set.seed(1)
trf.a <- Map(function(x, y) {
             rfe(x = x[, 1:floor(max.ad)]
                 , y = x[, y]
                 , sizes = 1:floor(max.ad)
                 , rfeControl = rf.ctrl
                 , ntree = 1000
                 , metric = 'ROC'
                 , trControl = ctrl)},
             x = adult.train, y = groups)
