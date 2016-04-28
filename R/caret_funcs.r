# various functions involved with supervised learning via caret
library(MASS)
library(nnet)
library(randomForest)
library(caret)
source('../R/multiclass_roc.r')

ctrl <- trainControl(method = 'repeatedcv',
                     classProbs = TRUE,
                     summaryFunction = multiClassSummary,
                     number = 5,
                     repeats = 10,
                     returnResamp = 'all')
