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
tadult.gap <- clusGap(adult.dist, FUNcluster = tpam,
                      K.max = 2 * n.groups, B = 500)
tadult.km <- pam(as.dist(adult.dist), k = which.max(tadult.gap$Tab[, 3]))

save(tadult.gap, tadult.km, file = '../data/cluster_res.RData')
