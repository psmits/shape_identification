#' Split three way array by factor
#'
#' Given a three way array and a factor vector, split the array into a list of three way arrays.
#'
#' @param shapes three-way array of shape data
#' @param f vector factor or character to break up shapes by
#' @return a list of three-way arrays
split.land <- function(shapes, f) {
  ll <- length(unique(f))
  for(jj in seq(ll)) {
    by.class <- list()
    sch <- as.character(f)
    sch.t <- unique(f)
    for(ii in seq(ll)) {
      sch.w <- sch == sch.t[ii]
      by.class[[ii]] <- shapes[, , sch.w]
    }
  }
  by.class
}


#' Eucldian distance matrix from shape data
#' 
#' Given a three-way array of shape data, calculate distance matrix of all pairwise comparisons.
#'
#' @param shapes three-way array of shape data
#' @return matrix of pairwise distances for all observations in shapes
land.dist <- function(shapes) {
  ns <- dim(shapes)[3]
  distmat <- matrix(nrow = ns, ncol = ns)
  for(ii in seq(ns)) {
    for(jj in seq(ns)) {
      distmat[ii, jj] <- sum(rowSums((shapes[, , ii] - shapes[, , jj])^2))
    }
  }
  distmat
}
