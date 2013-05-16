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
registerDoParallel(cores = detectCores())

source('../src/support_functions.r')
source('../src/randomize_funcs.r')

load('../data/turtle_analysis.RData')

ns <- 100

# multinomial
mod.num <- lapply(tm.analysis$sel, function(x) rownames(x[1, ]))
mod.num <- lapply(mod.num, as.numeric)
multi.form <- Map(function(x, n) x[[n]], x = tform, n = mod.num)
multi.sim <- sim.train(multi.form, turtle.train, nsim = ns, 
                       method = 'multinom', metric = 'ROC', 
                       trControl = ctrl, maxit = 1000)

mod.num <- lapply(tm.a.analysis$sel, function(x) rownames(x[1, ]))
mod.num <- lapply(mod.num, as.numeric)
multi.aform <- Map(function(x, n) x[[n]], x = tform.a, n = mod.num)
multia.sim <- sim.train(multi.aform, adult.train, nsim = ns,
                        method = 'multinom', metric = 'ROC',
                        trControl = ctrl, maxit = 1000)

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

save.image('../data/resample_runs.RData')
