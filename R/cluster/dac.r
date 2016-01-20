#' Clusting based on voting across multiple k values from PAM results.
#'
#' This function calculates the optimal number of clusterings in a given 
#' dataset according how well various clustering partitions work. Variant
#' on evidence accumulation clustering that accepts any distance/dissimilarity
#' matrix as unput. Potentially more ``robust.''
#'
#' @param dis distance/dissimilarity matrix to be clustered
#' @param krange vector of length 2 with range of k values to use for \code{\link{pam}}
#' @param iter number of iterations
#' @param hc heirarchical clustering method used by \code{\link{hclust}}
#' @keywords
#' @export
#' @examples
dac <- function(dis, krange, iter = 100, hc = "single") {
  coa.sim <- pam.coam(iter, krange, dis)

  coa.dist <- 1 - coa.sim
  hcm <- hclust(as.dist(coa.dist), method = hc)
  cut.val <- hcm$height[which.max(diff(hcm$height))]
  return(cutree(hcm, h = cut.val))
}

#' Support function for \code{\link{dac}}
#' 
#' Creates the coassociation matrix used by \code{\link{dac}}.
#'
#' @param iter number of iterations
#' @param krange range of k values to use for \code{\link{pam}}
#' @param data dataset to be clustered
#' @keywords
#' @examples
pam.coam <- function(iter, krange, dis) {
  nv <- dim(as.matrix(dis))[1]
  coa <- matrix(rep(0, nv * nv), nrow = nv)

  for (ii in seq(iter)) {
    jk <- sample(c(krange[1]:krange[2]), 1, replace = FALSE)
    jscl <- pam(x = dis, k = jk)$clustering
    coa.j <- matrix(rep(0, nv * nv), nrow = nv)
    for (jj in unique(jscl)) {
      indv <- which(jscl == jj)
      coa.j[indv, indv] <- coa.j[indv, indv] + (1 / iter)
    }
    coa <- coa + coa.j
  }
  return(coa)
}
