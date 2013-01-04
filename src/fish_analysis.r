###############################################################################
##
##  data analysis
##
##  ben frable
##  peter d smits
##
###############################################################################

## libraries
require(shapes)
require(geomorph)
require(cluster)

## source files
source('support_functions.r')
source('fish_mung.r')


## clustering
clust <- lapply(dists, clu)
