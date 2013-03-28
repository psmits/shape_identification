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
tmorph.gap <- clusGap(turtle.dist, FUNcluster = tpam, K.max = 2 * n.groups)
tmorph.km <- pam(as.dist(turtle.dist), k = which.max(tmorph.gap$Tab[, 3]))

# dissimilarity based evidence accumulation clustering
set.seed(1)
tmorph.dac <- dac(turtle.dist, krange = c(1, 10 * n.groups), iter = 1000)

save(tmorph.gap, tmorph.km, tmorph.dac, file = 'cluster_res.RData')
