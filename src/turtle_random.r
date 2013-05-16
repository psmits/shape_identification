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

nsim <- 100

# multinomial

# extract formulas
mod.num <- lapply(tm.analysis$sel, function(x) rownames(x[1, ]))
mod.num <- lapply(mod.num, as.numeric)
best.form <- Map(function(x, n) x[[n]], x = tform, n = mod.num)

out <- vector(mode = 'list', length = nsim)
for(ii in seq(nsim)) {
  out[[ii]] <- mapply(resample.train,
                      model = best.form,
                      data = turtle.train,
                      MoreArgs = list(method = 'multinom'
                                      , metric = 'ROC'
                                      , trControl = ctrl
                                      , maxit = 1000),
                      SIMPLIFY = FALSE)
}

out <- lapply(out, function(x) {
              names(x) <- groups
              x})

out <- roc.dist(tt)

save(out, '../data/turtle_random_test.RData')
