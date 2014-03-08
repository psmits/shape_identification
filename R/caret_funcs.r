# various functions involved with supervised learning via caret
library(MASS)
library(nnet)
library(randomForest)
library(caret)
source('../R/multiclass_roc.r')

ctrl <- trainControl(#method = 'LOOCV',
                     method = 'repeatedcv',
                     classProbs = TRUE,
                     summaryFunction = multiClassSummary,
                     number = 10,
                     repeats = 10,
                     returnResamp = 'all')

multiFuncs <- caretFuncs
multiFuncs$fit <- function(x, y, first, last, ...) {
  multinom(y ~ x, ...)
}

multiFuncs$summary <- multiClassSummary
multi.ctrl <- rfeControl(functions = multiFuncs,
                         method = 'repeatedcv',
                         repeats = 10,
                         number = 10,
                         verbose = FALSE,
                         returnResamp = 'final',
                         allowParallel = FALSE)

rfFuncs$summary <- multiClassSummary
rf.ctrl <- rfeControl(functions = rfFuncs,
                      method = 'repeatedcv',
                      repeats = 10,
                      number = 10,
                      verbose = FALSE, 
                      returnResamp = 'final'
                      )
