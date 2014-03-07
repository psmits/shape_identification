library(cluster)

clu <- function(x) {
# convenience clustering
#
#  Args:
#    x: square, symmetric distance matrix
#
#  Returns:
#    list with results from agnes and diana

  out <- list()
  out$agglo <- agnes(x)
  out$div <- diana(x)
  out
} 

