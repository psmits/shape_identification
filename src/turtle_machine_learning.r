###############################################################################
##
##  analysis of turtle plastron data
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

#RNGkind(kind = "L'Ecuyer-CMRG")
set.seed(1)

#registerDoParallel(cores = detectCores())

# source files
source('../src/support_functions.r')
source('../src/turtle_mung.r')


## unsupervised learning ##

# how many groups are there just from the geo reference info 
n.groups <- length(levels(turtle.meta$spinks))
tgeo.km <- pam(turtle.geo, k = n.groups)

# explore the range of clusterings
tgeo.gap <- clusGap(turtle.geo, FUNcluster = pam, K.max = 2 * n.groups)
tgeo.pam <- pam(turtle.geo, k = which.max(tgeo.gap$Tab[, 3]))

# cluster of the riemannian shape distances
# these methods are causing me to have...concerns
clust.meth <- list('complete', 'average', 'ward')
tmorph.hclust <- lapply(clust.meth, function(x) agnes(as.dist(turtle.dist),
                                                       method = x))
tmorph.coph <- lapply(tmorph.hclust, cophenetic)
tmorph.clcor <- lapply(tmorph.coph, function(x) cor(as.dist(turtle.dist), x,
                                                    method = 'spearman'))

# explore the range of clusterings
tpam <- function(x, k) pam(as.dist(x), k)
tmorph.gap <- clusGap(turtle.dist, FUNcluster = tpam, K.max = 2 * n.groups)
tmorph.km <- pam(as.dist(turtle.dist), k = which.max(tmorph.gap$Tab[, 3]))



## supervised learning ##

# classification methods
# multinomial logistic regression

max.var <- nrow(turtle.info) / 50
tvar <- paste('PC', 1:max.var, sep = '')
groups <- list('spinks', 'sh1', 'sh2', 'sh3')
tform <- lapply(groups, function(x, y) make.form(y, x), y = tvar)

in.train <- data.maker(unlist(groups), turtle.info)
turtle.train <- lapply(in.train, function(x) turtle.info[x, ])
turtle.test <- lapply(in.train, function(x) turtle.info[-x, ])

classes <- Map(function(x, y) colnames(x) %in% y, turtle.test, groups)
classes <- Map(function(x, y) x[, y], turtle.test, classes)

tmulti <- mapply(multi.train,
                 form = tform, data = turtle.train,
                 MoreArgs = list(method = 'multinom'
                                 , trControl = ctrl
                                 , maxit = 1000),
                 SIMPLFY = FALSE)

#tmulti.sel <- lapply(tmulti, function(mods) {
#                     model.sel(lapply(mods, function(x) x$finalModel))})
#names(tmulti.sel) <- unlist(groups)

# neural networks
tnnet <- mapply(multi.train,
                form = tform, data = turtle.train,
                MoreArgs = list(method = 'nnet'
                                , trControl = ctrl
                                , maxit = 1000),
                SIMPLFY = FALSE)

# averaged neural networks
tanet <- mapply(multi.train,
                form = tform, data = turtle.train,
                MoreArgs = list(method = 'avNNet'
                                , trControl = ctrl
                                , maxit = 1000),
                SIMPLFY = FALSE)

# random forest
tanet <- mapply(multi.train,
                form = tform, data = turtle.train,
                MoreArgs = list(method = 'rf'
                                , trControl = ctrl
                                , ntree = 1000
                                , importance = TRUE
                                ), 
                SIMPLFY = FALSE)


save.image(file = 'turtle_model_fits.RData')
