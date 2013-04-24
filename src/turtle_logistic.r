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
tmulti <- mapply(multi.train,
                 form = tform, data = turtle.train,
                 MoreArgs = list(method = 'multinom'
                                 , trControl = ctrl
                                 , maxit = 1000),
                 SIMPLFY = FALSE)

tmulti.a <- mapply(multi.train,
                   form = tform.a, data = turtle.train,
                   MoreArgs = list(method = 'multinom'
                                   , trControl = ctrl
                                   , maxit = 1000),
                   SIMPLIFY = FALSE)

save(tmulti, file = 'multi_boot_mod.RData')
