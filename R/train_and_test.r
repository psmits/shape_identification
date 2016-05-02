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
