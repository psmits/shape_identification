#' Evidence accumulation clustering.
#'
#' This function calculates the optimal number of clusters in a given dataset
#' according to how well various clustering partitions work. Uses multiple 
#' algorithms together to accomplish this. This code is based on a blog post
#' at things-about-r.tumblr.com.
#'
#' @param data dataset to be clustered
#' @param krange vector of length 2 with range of k values to use for \code{\link{kmeans}}
#' @param iter number of iterations
#' @param hc heirarchical clustering method used by \code{\link{hclust}}
#' @keywords
#' @export
#' @examples
eac <- function(data, krange, iter = 100, hc = "single") {
  coa.sim <- create.coam(iter, krange, data)

  coa.dist <- 1 - coa.sim
  hcm <- hclust(as.dist(coa.dist), method = hc)
  cut.val <- hcm$height[which.max(diff(hcm$height))]
  return(cutree(hcm, h = cut.val))
}

#' Support function for \code{\link{eac}}
#' 
#' Creates the coassociation matrix used by \code{\link{eac}}.
#'
#' @param iter number of iterations
#' @param krange range of k values to use for \code{\link{kmeans}}
#' @param data dataset to be clustered
#' @keywords
#' @examples
create.coam <- function(iter, krange, data) {
  nv <- dim(data)[1]
  coa <- matrix(rep(0, nv * nv), nrow = nv)

  for (ii in seq(iter)) {
    jk <- sample(c(krange[1]:krange[2]), 1, replace = FALSE)
    jscl <- kmeans(x = data, centers = jk)$cluster
    coa.j <- matrix(rep(0, nv * nv), nrow = nv)
    for (jj in unique(jscl)) {
      indv <- which(jscl == jj)
      coa.j[indv, indv] <- coa.j[indv, indv] + (1 / iter)
    }
    coa <- coa + coa.j
  }
  return(coa)
}
