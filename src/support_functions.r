###############################################################################
##
##  support functions
##
##  peter d smits
##
###############################################################################

## libraries
require(utils)
require(shapes)
require(geomorph)

## functions

riem.matrix <- function(config) {
  ##  create a pair-wise Riemannian distance matrix
  ##
  ##  Args:
  ##    config: k x m x n array of (fit) landmarks
  ##
  ##  Returns:
  ##    an n x n symetric matrix of Riemannian shape distances

  n <- dim(config)[3]

  pr <- combn(n, 2)
  dis <- apply(pr, 2, function(x) riemdist(config[, , x[1]], 
                                           config[, , x[2]]))
  mat <- matrix(ncol = n, nrow = n)
  for(ii in seq(ncol(pr))) {
    mat[pr[1, ii], pr[2, ii]] <- dis[ii]
  }

  mat[lower.tri(mat)] <- t(mat[upper.tri(mat)])
  diag(mat) <- 0

  mat  
}

clu <- function(x) {
  ## convenience clustering
  ##
  ##  Args:
  ##    x: square, symmetric distance matrix
  ##
  ##  Returns:
  ##    list with results from agnes and diana

  out <- list()
  out$agglo <- agnes(x)
  out$div <- diana(x)
  out
} 

land.frame <- function(x) {
  ## make a nice data frame from a 3-dimenstional array
  ##
  ##  Args:
  ##    x: 3-way array
  ##
  ##  Returns:
  ##    data frame that is useful with ggplot

  m <- dim(x)[2]
  n <- dim(x)[3]
  dat <- x[, , 1]
  for (ii in seq(from = 2, to = n)) {
    dat <- rbind(dat, x[, , ii])
  }

  dat <- as.data.frame(dat)
  if (m == 2) nam <- c('x', 'y') else if (m == 3) nam <- c('x', 'y', 'z')
  names(dat) <- nam

  dat
}
