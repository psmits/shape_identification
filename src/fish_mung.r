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
require(abind)

## source files
source('../src/support_functions.r')

## data
ano.land <- readland.tps(file = '../data/AnostomidaeFinal.TPS')
cur.land <- readland.tps(file = '../data/CurimatidaeF.TPS')
cur.land <- cur.land[, , -24]  # there is an outlier
pro.land <- readland.tps(file = '../data/ProchilodontidaeF.TPS')
chi.land <- readland.tps(file = '../data/Chilodontidae2.TPS')

fish.land <- abind::abind(ano.land, cur.land, pro.land, chi.land)
dimnames(fish.land)[[3]] <- c(dimnames(ano.land)[[3]], 
                              dimnames(cur.land)[[3]],
                              dimnames(pro.land)[[3]],
                              dimnames(chi.land)[[3]])

name <- c('ano', 'cur', 'pro', 'chi', 'fish')

## generalized procrustes fit
## PCA of the GPA fit landmarks
fits <- lapply(list(ano.land, cur.land,
                    pro.land, chi.land,
                    fish.land), procGPA)
names(fits) <- name

## family means
## need to automate this better
ano.mean <- mshape(fits$fish$rotated[, , dim(ano.land)[3]])
cur.mean <- mshape(fits$fish$rotated[, , dim(cur.land)[3]])
pro.mean <- mshape(fits$fish$rotated[, , dim(pro.land)[3]])
chi.mean <- mshape(fits$fish$rotated[, , dim(chi.land)[3]])

## distance matrices
dists <- lapply(fits, function(x) riem.matrix(x$rotated))
names(dists) <- name
