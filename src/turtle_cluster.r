###############################################################################
##
##  analysis of turtle plastron data
##  unsupervised learning approaches
##
##  peter d smits
##  psmits@uchicago.edu
##
###############################################################################

# libraries
require(cluster)
set.seed(1)


# source files
source('../src/support_functions.r')
source('../src/turtle_mung.r')
source('../src/dac.r') 


## unsupervised learning ##

# explore the range of clusterings for the morphology
#
# gap clustering
n.groups = 4

set.seed(1)
tpam <- function(x, k) pam(as.dist(x), k)
tmorph.gap <- clusGap(turtle.info.dist, FUNcluster = tpam, 
                      K.max = 10 * n.groups, B = 500)
tadult.gap <- clusGap(turtle.adult.dist, FUNcluster = tpam,
                      K.max = 10 * n.groups, B = 500)
tmorph.km <- pam(as.dist(turtle.info.dist), k = which.max(tmorph.gap$Tab[, 3]))
tadult.km <- pam(as.dist(turtle.adult.dist), k = which.max(tadult.gap$Tab[, 3]))

# dissimilarity based evidence accumulation clustering
set.seed(1)
tmorph.dac <- dac(turtle.info.dist, krange = c(1, 50 * n.groups), iter = 10000)
tadult.dac <- dac(turtle.adult.dist, krange = c(1, 50 * n.groups), iter = 10000)

save(tmorph.gap, tmorph.km, tmorph.dac, 
     tadult.gap, tadult.km, tadult.dac, 
     file = 'cluster_res.RData')
