#  create a pair-wise Riemannian distance matrix
#
#  Args:
#    config: k x m x n array of (fit) landmarks
#
#  Returns:
#    an n x n symetric matrix of Riemannian shape distances
riem.matrix <- function(config) {
  n <- dim(config)[3]

  pr <- combn(n, 2)
  dis <- apply(pr, 2, function(x) ssriemdist(config[, , x[1]], 
                                             config[, , x[2]]))
  mat <- matrix(ncol = n, nrow = n)
  for(ii in seq(ncol(pr))) {
    mat[pr[1, ii], pr[2, ii]] <- dis[ii]
  }

  mat[lower.tri(mat)] <- t(mat[upper.tri(mat)])
  diag(mat) <- 0

  mat  
}
