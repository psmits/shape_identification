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
source('../src/turtle_mung.r')


## supervised learning ##

max.var <- nrow(turtle.info) / 50
max.ad <- nrow(turtle.adult) / 50
tvar <- paste('PC', 1:max.var, sep = '')
tvar.s <- c('lat', 'long', tvar)
tvar.a <- paste('PC', 1:max.ad, sep = '')
tvar.a.s <- c('lat', 'long', tvar.a)
groups <- list('sh1', 'sh2', 'sh3', 'spinks')
tform <- lapply(groups, function(x, y) make.form(y, x), y = tvar)
tform.s <- lapply(groups, function(x, y) make.form(y, x), y = tvar.s)
tform.a <- lapply(groups, function(x, y) make.form(y, x), y = tvar.a)
tform.a.s <- lapply(groups, function(x, y) make.form(y, x), y = tvar.a.s)

# including juvies
set.seed(1)
in.train <- as.data.frame(data.maker(unlist(groups), turtle.info))
turtle.train <- lapply(in.train, function(x) turtle.info[x, ])
turtle.test <- lapply(in.train, function(x) turtle.info[-x, ])

classes <- Map(function(x, y) colnames(x) %in% y, turtle.test, groups)
classes <- Map(function(x, y) x[, y], turtle.test, classes)

# excluding juvies
set.seed(1)
ad.train <- data.maker(unlist(groups), turtle.adult)
adult.train <- lapply(ad.train, function(x) turtle.adult[x, ])
adult.test <- lapply(ad.train, function(x) turtle.adult[-x, ])

ad.class <- Map(function(x, y) colnames(x) %in% y, adult.test, groups)
ad.class <- Map(function(x, y) x[, y], adult.test, ad.class)

turtle.design <- Map(function(x, y) {
                     cbind(lat = x$lat, long = x$long,
                           x[, 1:max.var],
                           cate = x[, y])},
                     x = turtle.train, y = groups)
adult.design <- Map(function(x, y) {
                    cbind(lat = x$lat, long = x$long,
                          x[, 1:max.ad],
                          cate = x[, y])},
                    x = adult.train, y = groups)

# O
save.image('../data/supervised_misc.RData')
