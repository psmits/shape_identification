# unsupervised learning approaches
#
# peter d smits
# psmits@uchicago.edu
###############################################################################

require(cluster)

source('../R/clus_func.r')
source('../R/mung.r')
source('../R/dac.r') 

# explore the range of clusterings for the morphology
# gap clustering
n.groups = 4

set.seed(1)
tpam <- function(x, k) pam(as.dist(x), k)
tmorph.gap <- clusGap(info.dist, FUNcluster = tpam, 
                      K.max = 10 * n.groups, B = 500)
tadult.gap <- clusGap(adult.dist, FUNcluster = tpam,
                      K.max = 10 * n.groups, B = 500)
tmorph.km <- pam(as.dist(info.dist), k = which.max(tmorph.gap$Tab[, 3]))
tadult.km <- pam(as.dist(adult.dist), k = which.max(tadult.gap$Tab[, 3]))

# dissimilarity based evidence accumulation clustering
set.seed(1)
tmorph.dac <- dac(info.dist, krange = c(1, 50 * n.groups), iter = 10000)
tadult.dac <- dac(adult.dist, krange = c(1, 50 * n.groups), iter = 10000)

save(tmorph.gap, tmorph.km, tmorph.dac, 
     tadult.gap, tadult.km, tadult.dac, 
     file = 'cluster_res.RData')
