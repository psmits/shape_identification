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

load('../data/supervised_misc.RData')

set.seed(1)
trf <- Map(function(x, y) {
           rfe(x = x[, 1:floor(max.var)]
               , y = x[, y]
               , sizes = 1:floor(max.var)
               , rfeControl = rf.ctrl
               , ntree = 1000
               , metric = 'ROC'
               , trControl = ctrl)},
           x = turtle.train, y = groups)
set.seed(1)
trf.s <- Map(function(x) {
             rfe(x = x[, -ncol(x)]
                 , y = x$cate
                 , sizes = 3:(ncol(x) - 1)
                 , rfeControl = rf.ctrl
                 , ntree = 1000
                 , metric = 'ROC'
                 , trControl = ctrl)}, 
             x = turtle.design)

set.seed(1)
trf.a <- Map(function(x, y) {
             rfe(x = x[, 1:floor(max.ad)]
                 , y = x[, y]
                 , sizes = 1:floor(max.ad)
                 , rfeControl = rf.ctrl
                 , ntree = 1000
                 , metric = 'ROC'
                 , trControl = ctrl)},
             x = adult.train, y = groups)
set.seed(1)
trf.a.s <- Map(function(x) {
             rfe(x = x[, -ncol(x)]
                 , y = x$cate
                 , sizes = 3:(ncol(x) - 1)
                 , rfeControl = rf.ctrl
                 , ntree = 1000
                 , metric = 'ROC'
                 , trControl = ctrl)},
             x = adult.design)


save(trf, trf.s,
     trf.a, trf.a.s, 
     file = '../data/rf_boot_mod.RData')
