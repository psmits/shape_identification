##############################################################################
##
##  analysis of turtlee plastron data
##  randomization of the model fits to create a distribution of AUC ROC
##
##  peter d smits
##  psmits@uchicago.edu
##
###############################################################################

require(plyr)
require(cluster)
require(nnet)
require(randomForest)
require(caret)
require(e1071)
require(pROC)
require(MuMIn)

source('../src/support_functions.r')

#load('../data/turtle_analysis.RData')

# randomly reassign the classes of the training data set
# refit the best sized model to the permuted training set
# this creates a distribution of ROC values against "random"
#
# are the models better than random?
# can then see if ROC is significant

#' Create distribution of resampling values
#'
#' TODO allow of x and y instead of just formula
#'
#' @param model object of class formula
#' @param data object of class data.frame for use with formula as training data
#' @param nsim number of resamples to compute 
#' @param ... additional arguments passed to caret::train
sim.train <- function(model, data, nsim, ...) {
  out <- vector(mode = 'list', length = nsim)
  for(ii in seq(nsim)) {
    out[[ii]] <- mapply(resample.train,
                        model = model,
                        data = data,
                        MoreArgs = list(...),
                        SIMPLIFY = FALSE)
  }

  out <- lapply(out, function(x) {
                names(x) <- names(data)
                x})

  out
}


resample.class <- function(data, class) {
  data[, class] <- sample(data[, class])
  data
}

#' Train model to resampled data.
#'
#' Resampling is done internally.
#'
#' TODO make it so you don't require the formula but can just 
#' take x and y arguments
#'
#' @param model object of class formula suitable for caret::train function
#' @param data object of class data frame which model uses
#' @param ... arguments passed to caret:train
resample.train <- function(model, data, ...) {
  if('formula' %in% class(model)) {
    css <- all.vars(model)[1]
  }

  new.data <- resample.class(data, css)
  new.train <- train(form = model, data = new.data, ...)

  out <- list()
  out$resamp <- new.data
  out$retrain <- new.train

  out
}

#' Create distribution of ROC 
#'
#' @param resamples list of outputs from resample.train
roc.dist <- function(resamples, spl = FALSE) {
  # extract ROC value from every element of a list
  dis <- lapply(resamples, function(x) {
                lapply(x, function(y) {
                       max(y$retrain$results$ROC)})})
  dis <- melt(dis)

  if(spl) {
    dis <- split(dis, dis$L2)
  }

  dis
}

#' predict class from resamples
#'
predict.dist <- function(models, test, ...) {
  preds <- lapply(models, function(x, y) {
                  predict(object = x, newdata = y, ...)},
                  y = test)
  preds
}
