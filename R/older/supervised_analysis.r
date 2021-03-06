# fit supervised learning models
#
# peter d smits
# psmits@uchicago.edu
###############################################################################
library(MASS)
library(nnet)
library(randomForest)
library(caret)
library(parallel)
library(doParallel)

source('../R/support_functions.r')
source('../R/caret_funcs.r')  # helper functions for train
source('../R/supervised_mung.r')  # training and testing data

RNGkind(kind = "L'Ecuyer-CMRG")
registerDoParallel(cores = detectCores())
set.seed(1)

# linear discriminate 
set.seed(1)
tlda.a <- Map(function(x, g) {
              out <- list()
              for(ii in seq(max.ad)) {
                out[[ii]] <- lda(x[seq(ii)], x[, g])
              }
              out},
              x = adult.train, g = groups)


# multinomial logistic regression
set.seed(1)
tmulti.a <- Map(function(x, g) {
              out <- list()
              for(ii in seq(length(g))) {
                out[[ii]] <- multinom(g[[ii]], data = x)
              }
              out},
              x = adult.train, g = tform.a)
  
# random forests
set.seed(1)
trf.a <- Map(function(x, y) {
             rfe(x = x[, 1:floor(max.ad)]
                 , y = x[, y]
                 , sizes = 1:floor(max.ad)
                 , rfeControl = rf.ctrl
                 , ntree = 100
                 , metric = 'ROC')},
             x = adult.train, y = groups)
for.imp <- Map(function(x, y) {
               randomForest(x = x[, 1:floor(max.ad)]
                            , y = x[, y]
                            , ntree = 100
                            , importance = TRUE)},
               x = adult.train, y = groups)
