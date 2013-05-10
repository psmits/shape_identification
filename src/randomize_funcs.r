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

resample.class <- function(data, class) {
  data[, class] <- sample(data[, class])
  data
}

# write these as more generic
resample.rf <- function(model, training, class) {
  re.tr <- resample.class(data = training, class = class)
  new.train <- train(form = as.formula(paste0(class, '~',
                                       paste(model$optVariables, 
                                             collapse = '+'))),
                     data = re.tr,
                     method = 'rf',
                     metric = 'ROC',
                     trControl = ctrl,
                     ntree= 10000)

  new.train
}

resample.multi <- function(model, training, class) {
  re.tr <- resample.class(data = training, class = class)
  new.train <- train(form ,
                     data = re.tr,
                     method = 'mulinom',
                     metric = 'ROC',
                     trControl = ctrl,
                     maxit = 10000)

  new.train
}


roc.dist <- function(models) {
}

predict.dist <- function(models, test) {
}
