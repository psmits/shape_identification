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
  out$sig <- sum(emp > dd) / n
  out
}

# are all elements identical?
zero.range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

test.all <- function(analysis, test, group){
  tests <- list()
  for(ii in seq(length(group))) {
    config <- split(test[[ii]][,1:26], test[[ii]][, group[[ii]]])
    mis.mat <- Map(which.right,
                   split(analysis$class[[ii]][, 1], test[[ii]][, group[[ii]]]),
                   split(test[[ii]][, group[[ii]]], test[[ii]][, group[[ii]]]))

    bad <- laply(mis.mat, function(x) length(x) < 10 | zero.range(x))
    config <- config[!bad]
    mis.mat <- mis.mat[!bad]

    tests[[ii]] <- Map(function(x, y) centroid.perm(x, y), mis.mat, config)
    tests[[ii]]$rm <- which(bad)
  }
  tests
}

rf.test <- test.all(trf.a.analysis, adult.test, groups)
mm.test <- test.all(tm.a.analysis, adult.test, groups)
ll.test <- test.all(tl.a.analysis, adult.test, groups)
