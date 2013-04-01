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

# random forest
set.seed(1)
trf <- mapply(multi.train,
                form = tform, data = turtle.train,
                MoreArgs = list(method = 'rf'
                                , trControl = ctrl
                                , ntree = 1000
                                , importance = TRUE
                                ), 
              SIMPLIFY = FALSE)
save(trf, file = 'rf_boot_mod.RData')

