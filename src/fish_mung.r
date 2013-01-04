###############################################################################
##
##  data munging
##
##  ben frable
##  peter d smits
##
###############################################################################

## libraries
require(shapes)
require(geomorph)
require(reshape2)

## source files
source('support_functions.r')

## data
ano.land <- readland.tps(file = '../data/AnostomidaeFinal.TPS')
cur.land <- readland.tps(file = '../data/CurimatidaeF.TPS')
cur.land <- cur.land[, , -24]  # there is an outlier

fish.land <- abind(ano.land, cur.land)
dimnames(fish.land)[[3]] <- c(dimnames(ano.land)[[3]], dimnames(cur.land)[[3]])

name <- c('ano', 'cur', 'fish')

## generalized procrustes fit
## PCA of the GPA fit landmarks
fits <- lapply(list(ano.land, cur.land, fish.land), procGPA)
names(fits) <- name

## family means
## need to automate this better
ano.mean <- mshape(fits$fish$rotated[, , dim(ano.land)[3]])
cur.mean <- mshape(fits$fish$rotated[, , dim(cur.land)[3]])

## distance matrices
dists <- lapply(fits, function(x) riem.matrix(x$rotated))
names(dists) <- name
