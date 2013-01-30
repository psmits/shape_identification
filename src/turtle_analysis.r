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

# how many groups are there just from the geo reference info 
n.groups <- length(levels(turtle.meta$spinks))
tgeo.km <- pam(turtle.geo, k = n.groups)
tgeo.fuzzy <- fanny(turtle.geo, k = n.groups)
# quick visual
#clusplot(turtle.km, color = TRUE, shade = TRUE, lines = FALSE)

# explore the range of clusterings
tgeo.gap <- clusGap(turtle.geo, FUNcluster = fanny, K.max = n.groups)


# cluster of the riemannian shape distances
# these methods are causing me to have...concerns
clust.meth <- list('complete', 'average', 'ward')
tmorph.hclust <- lapply(clust.meth, function(x) agnes(as.dist(turtle.dist),
                                                       method = x))
tmorph.coph <- lapply(tmorph.hclust, cophenetic)
tmorph.clcor <- lapply(tmorph.coph, function(x) cor(as.dist(turtle.dist), x,
                                                    method = 'spearman'))

tmorph.km <- pam(as.dist(turtle.dist), k = n.groups)

# explore the range of clusterings
tpam <- function(x, k) pam(as.dist(x), k)
tmorph.gap <- clusGap(turtle.dist, FUNcluster = tpam, K.max = n.groups)



# classification methods
# multinomial logistic regression
# neural nets
# random forests

max.var <- nrow(turtle.info) / 50
tform <- vector(mode = 'list', length = max.var)
tvar <- paste('PC', 1:max.var, sep = '')
for (ii in seq(max.var)) {
  tform[[ii]] <- as.formula(paste('spinks ~ ', 
                                  paste(tvar[seq(ii)],
                                        collapse = '+')))
}

method <- list('multinom')
               #'nnet', 
               #'randomForest'
groups <- c('spinks', 'sh1', 'sh2', 'sh3')

in.train <- data.maker(groups, turtle.info)
turtle.train <- lapply(in.train, function(x) turtle.info[x, ])
turtle.test <- lapply(in.train, function(x) turtle.info[-x, ])


# list of methods
# list of data sets
# list of formulas

# for each data set, try all formulas, and for each method

