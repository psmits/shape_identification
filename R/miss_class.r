library(shapes)
library(geomorph)
library(plyr)
library(reshape2)

load('../data/gen.RData')

# which observations are missclassified
which.right <- function(cl, ac) {
  right <- which(cl == ac)
  ww <- rep(0, length(ac))
  ww[right] <- 1
  ww
}

# permutation test. 
# get (PCA) centroid of wrongs and rights.
#   mean on every axis
#   euclidean distance 
# permute lables N times and see if difference is significant.
get.centroid <- function(axes) {
  apply(axes, 2, mean)
}
cen.euc <- function(x, y) {
  sqrt(sum((x - y)^2))
}

centroid.perm <- function(label, axes, n = 1000) {
  eg <- split(axes, label)
  ec <- lapply(eg, get.centroid)
  emp <- cen.euc(ec[[1]], ec[[2]])
  
  dd <- c()
  for(ii in seq(n)) {
    nl <- sample(label)
    gr <- split(axes, nl)
    cents <- lapply(gr, get.centroid)
    dd[ii] <- cen.euc(cents[[1]], cents[[2]])
  }

  out <- list()
  out$empircal <- emp
  out$distibution <- dd
  out$sig <- sum((emp - dd) > 0) / n
  out
}

miss.match <- which.right(trf.a.analysis$class[[1]]$pred, 
                          adult.test[[1]][, groups[[1]]])

test <- centroid.perm(miss.match, adult.test[[1]][, 1:26])


#trf.a.analysis

#tm.a.analysis

#tl.a.analysis
