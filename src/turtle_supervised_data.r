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
source('../src/turtle_mung.r')


## supervised learning ##

max.var <- nrow(turtle.info) / 50
tvar <- paste('PC', 1:max.var, sep = '')
groups <- list('spinks', 'sh1', 'sh2', 'sh3')
tform <- lapply(groups, function(x, y) make.form(y, x), y = tvar)

set.seed(1)
in.train <- data.maker(unlist(groups), turtle.info)
turtle.train <- lapply(in.train, function(x) turtle.info[x, ])
turtle.test <- lapply(in.train, function(x) turtle.info[-x, ])

classes <- Map(function(x, y) colnames(x) %in% y, turtle.test, groups)
classes <- Map(function(x, y) x[, y], turtle.test, classes)

save.image('supervised_misc.RData')
