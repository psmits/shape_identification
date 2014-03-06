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

require(parallel)
require(doParallel)

RNGkind(kind = "L'Ecuyer-CMRG")
set.seed(1)
registerDoParallel(cores = 2)

source('../src/support_functions.r')
source('../src/randomize_funcs.r')

load('../data/turtle_analysis.RData')

ns <- 100

ww <- lapply(trf, function(x) x$bestSubset)
rf.form <- Map(function(x, n) x[[n]], x = tform, n = ww)
rf.sim <- sim.train(rf.form, turtle.train, nsim = ns,
                    method = 'rf', metric = 'ROC',
                    trControl = ctrl, ntree = 1000)

ww <- lapply(trf.a, function(x) x$bestSubset)
rf.aform <- Map(function(x, n) x[[n]], x = tform.a, n = ww)
rfa.sim <- sim.train(rf.aform, adult.train, nsim = ns,
                     method = 'rf', metric = 'ROC',
                     trControl = ctrl, ntree = 1000)

save(rf.sim, rfa.sim,
     file = '../data/rf_random.RData')
