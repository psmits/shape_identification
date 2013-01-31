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
require(MuMIn)

# source files
source('../src/support_functions.r')
source('../src/turtle_mung.r')

set.seed(1)

## unsupervised learning ##

# how many groups are there just from the geo reference info 
n.groups <- length(levels(turtle.meta$spinks))
tgeo.km <- pam(turtle.geo, k = n.groups)

# explore the range of clusterings
tgeo.gap <- clusGap(turtle.geo, FUNcluster = fanny, K.max = n.groups)
tgeo.fuzzy <- fanny(turtle.geo, k = which.max(tgeo.gap$Tab[, 3]))

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
tmorph.gap <- clusGap(turtle.dist, FUNcluster = tpam, K.max = n.groups)
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

tmulti <- mapply(across.part.train, tform, turtle.train, 
                       MoreArgs = list(method = 'multinom'), 
                       SIMPLIFY = FALSE)

tmulti.sel <- lapply(tmulti, function(mods) {
                     model.sel(lapply(mods, function(x) x$finalModel))})
names(tmulti.sel) <- unlist(groups)

# support vector machines
tsvm <- Map(function(x, y) lapply(x, svm, y), tform, turtle.train)
tsvm.pred <- lapply(turtle.svm, function(x) lapply(x, predict))


# neural nets
#turtle.nnet <- mapply(across.part.train, tform, turtle.train,
#                      MoreArgs = list(method = 'nnet', size = 2),
#                      SIMPLIFY = FALSE)

# random forest
