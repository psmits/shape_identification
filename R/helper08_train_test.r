library(pacman)

p_load(here)

# wrapper for all of my model fitting exercises
use.model <- function(method, adult, scheme, npred = 28) {
  source(here::here('R', 'helper03_caret_funcs.r'))
  # create data partition for training set and testing set
  trainsets <- testsets <- list()
  train.res <- test.res <- list()
  # methods

  #method <- meth[1]
  for(kk in seq(length(scheme))) {
    rms <- is.na(adult[, scheme[kk]]) | adult[, scheme[kk]] == ''
    adult1 <- adult[!rms, ]

    schism <- createDataPartition(adult1[, scheme[kk]], p = 0.8,
                                  list = FALSE, times = 1)

    trainsets[[kk]] <- adult1[schism, ]
    testsets[[kk]] <- adult1[-schism, ]

    dataset <- trainsets[[kk]][, 1:npred]
    responses <- trainsets[[kk]][, scheme[kk]]

    # do cross validation of ROC for all random forest models
    traind <- list()
    for(ii in seq(from = 4, to = npred)) {
      traind[[ii - 3]] <- 
        train(y = responses, 
              x = dataset[, seq(ii)],
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


abrv.model <- function(method, adult, scheme, npred = 28) {		

  part <- llply(adult, function(x) 		
                createDataPartition(x[, 1], p = 0.8, list = FALSE, times = 1))		

  trainsets <- testsets <- list()		
  train.res <- test.res <- list()		

  train.res <- test.res <- list()		
  for(kk in seq(length(scheme))) {		
    trainsets[[kk]] <- adult[[kk]][part[[kk]], ]		
    testsets[[kk]] <- adult[[kk]][-part[[kk]], ]		

    dataset <- trainsets[[kk]][, 2:(npred + 1)]		
    responses <- trainsets[[kk]][, 1]		

    traind <- list()		
    for(ii in seq(from = 4, to = npred)) {		
      traind[[ii - 3]] <- 		
        train(y = responses, 		
              x = dataset[, seq(ii)],		
              method = method,		
              trControl = ctrl,		
              metric = 'ROC', 		
              tuneLength = 1)		
    }		
    train.res[[kk]] <- traind		


    # now test all the models against external data		
    pclass <- llply(traind, function(x) 		
                    predict.train(object = x, 		
                                  newdata = testsets[[kk]][, 2:(npred + 1)], 		
                                  type = 'raw'))		
    prob <- llply(traind, function(x)		
                  predict.train(object = x, 		
                                newdata = testsets[[kk]][, 2:(npred + 1)], 		
                                type = 'prob'))		
    test.res[[kk]] <- Map(function(x, y) cbind(class = x, y), pclass, prob)		
  }		

  final.res <- list(training = train.res, testing = test.res,		
                    training.dataset = trainsets, testing.dataset = testsets)		
  final.res		
}

