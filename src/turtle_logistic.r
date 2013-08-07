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
# change to be the design format and using recursive feature selection
#tmulti <- mapply(multi.train,
#                 form = tform, data = turtle.train,
#                 MoreArgs = list(method = 'multinom'
#                                 , metric = 'ROC'
#                                 , trControl = ctrl
#                                 , maxit = 1000),
#                 SIMPLFY = FALSE)
#set.seed(1)
#tmulti.s <- mapply(multi.train,
#                   form = tform.s, data = turtle.train,
#                   MoreArgs = list(method = 'multinom'
#                                   , metric = 'ROC'
#                                   , trControl = ctrl
#                                   , maxit = 1000),
#                   SIMPLFY = FALSE)

set.seed(1)
tmulti.a <- mapply(multi.train,
                   form = tform.a, data = adult.train,
                   MoreArgs = list(method = 'multinom'
                                   , metric = 'ROC'
                                   , trControl = ctrl
                                   , maxit = 1000),
                   SIMPLIFY = FALSE)
set.seed(1)
tmulti.a.s <- mapply(multi.train,
                     form = tform.a.s, data = adult.train,
                     MoreArgs = list(method = 'multinom'
                                     , metric = 'ROC'
                                     , trControl = ctrl
                                     , maxit = 1000),
                     SIMPLIFY = FALSE)

save(#tmulti, tmulti.s,
     tmulti.a, tmulti.a.s,
     file = '../data/multi_boot_mod.RData')
