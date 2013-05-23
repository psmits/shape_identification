##############################################################################
##
##  analysis of turtlee plastron data
##  analysis of machine learning results
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
source('../src/randomize_funcs.r')

load('../data/turtle_analysis.RData')
load('../data/rf_random.RData')

rf.roc <- roc.dist(rf.sim)
rfa.roc <- roc.dist(rfa.sim)
