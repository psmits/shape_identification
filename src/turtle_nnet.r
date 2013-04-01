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

# neural networks
set.seed(1)
tnnet <- mapply(multi.train,
                form = tform, data = turtle.train,
                MoreArgs = list(method = 'nnet'
                                , trControl = ctrl
                                , maxit = 1000),
                SIMPLFY = FALSE)
save(tnnet, file = 'nnet_boot_mod.RData')
