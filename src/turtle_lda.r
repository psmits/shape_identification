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
tlda.a <- Map(function(x, g) {
              out <- list()
              for(ii in seq(max.ad)) {
                out[[ii]] <- train(x[seq(ii)], x[, g], method = 'lda',
                                   metric = 'ROC', trControl = ctrl)
              }
              out},
              x = adult.train, g = groups)

save(tlda.a, 
     file = '../data/lda_boot_mod.RData')
