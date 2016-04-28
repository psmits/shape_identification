library(plyr)
library(MASS)
library(nnet)
library(randomForest)
library(mda)
library(kernlab)
library(caret)
library(parallel)
library(doParallel)
source('../R/caret_funcs.r')
source('../R/multiclass_roc.r')

# wrapper for all of my analyses
use.model <- function(method, adult, scheme, npred = 28) {
  # create data partition for training set and testing set
  schism <- alply(scheme, 1, function(x) 
                  createDataPartition(adult[, x], p = 0.8, 
                                      list = FALSE, times = 1))

  trainsets <- testsets <- list()
  train.res <- test.res <- list()
  # methods

  for(kk in seq(length(scheme))) {
    trainsets[[kk]] <- adult[schism[[kk]], ]
    testsets[[kk]] <- adult[-schism[[kk]], ]

    dataset <- trainsets[[kk]][, 1:npred]
    responses <- trainsets[[kk]][, scheme[kk]]

    # do cross validation of ROC for all random forest models
    traind <- list()
    for(ii in seq(from = 3, to = ncol(dataset))) {
      traind[[ii - 2]] <- 
        train(form = responses ~ ., 
              data = as.matrix(dataset[, seq(ii)]),
              method = method,
              trControl = ctrl, 
              metric = 'ROC', 
              tuneLength = 1)
    }
    train.res[[kk]] <- traind

    # now test all the models against external data
    pclass <- llply(traind, function(x) 
                    predict.train(object = x, 
                                  newdata = testsets[[kk]][, 1:npred], 
                                  type = 'raw'))
    prob <- llply(traind, function(x)
                  predict.train(object = x, 
                                newdata = testsets[[kk]][, 1:npred], 
                                type = 'prob'))
    test.res[[kk]] <- Map(function(x, y) cbind(class = x, y), pclass, prob)
  }

  final.res <- list(training = train.res, testing = test.res,
                    training.dataset = trainsets, testing.dataset = testsets)
  final.res
}


# start analysis
registerDoParallel(cores = detectCores())

source('../R/supervised_mung.r')
adult$spinks <- LETTERS[adult$spinks]

schemes <- c('sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks')
meth <- c('multinom', 'nnet', 'lda', 'pda', 'rf')

results <- list()
for(ii in seq(length(meth))) {
  results[[ii]] <- use.model(method = meth[ii],
                             adult = adult, 
                             scheme = schemes,
                             npred = 25)
}


#oo <- use.model(method = meth[1], adult = adult, scheme = schemes)
#llply(oo$training, function(x) laply(x, function(y) y$results[c('ROC', 'ROCSD')]))


# use to calculate ROC using allvone
#get.testing <- Map(function(y, z, w) 
#                   laply(y, function(x) 
#                         allvone(x, oo$testing.dataset[[w]][, z])), 
#                   y = oo$testing, z = schemes, w = length(schemes))

