###############################################################################
##
##  analysis of turtle plastron data
##  supervised learning approaches
##
##  peter d smits
##  psmits@uchicago.edu
##
###############################################################################

# libraries
require(MASS)
require(nnet)
require(cluster)
require(e1071)
require(caret)
require(randomForest)
require(pROC)
require(MuMIn)

require(parallel)
require(doParallel)

RNGkind(kind = "L'Ecuyer-CMRG")
set.seed(1)

registerDoParallel(cores = detectCores())

# source files
source('../src/support_functions.r')

load('../src/supervised_misc.RData')

set.seed(1)
trf <- Map(function(x, y) {
           rfe(x = x[, 1:floor(max.var)]
               , y = x[, y]
               , sizes = 1:floor(max.var)
               , rfeControl = rf.ctrl
               , ntree = 1000
               , trControl = ctrl)}, 
           x = turtle.train, y = groups)

set.seed(1)
trf.a <- Map(function(x, y) {
             rfe(x = x[, 1:floor(max.ad)]
                 , y = x[, y]
                 , sizes = 1:floor(max.ad)
                 , rfeControl = rf.ctrl
                 , ntree = 1000
                 , trControl = ctrl)},
             x = adult.train, y = groups)


save(trf, trf.a, file = 'rf_boot_mod.RData')

