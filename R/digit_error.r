library(shapes)
library(geomorph)
source('../R/df2array.r')
source('../R/supervised_mung.r')

by.scheme <- list()
scheme <- c('sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks')
for(jj in seq(6)) {
  by.class <- list()
  sch <- as.character(adult[, scheme[jj]])
  sch.t <- unique(sch)
  for(ii in seq(length(sch.t))) {
    sch.w <- sch == sch.t[ii]
    by.class[[ii]] <- fit$rotated[, , sch.w]
  }
  by.scheme[[jj]] <- by.class
}

class.dist <- list()
for(ii in seq(length(by.scheme))) {
  class.mean <- llply(by.scheme[[ii]], mshape)
  distmat <- matrix(nrow = length(by.scheme[[ii]]), 
                    ncol = length(by.scheme[[ii]]))
  for(jj in seq(length(class.mean))) {
    for(kk in seq(length(class.mean))) {
      distmat[jj, kk] <- sum(rowSums((class.mean[[jj]] - class.mean[[kk]])^2))
    }
  }
  class.dist[[ii]] <- distmat
}
# matrix of distances between centroids of schemes



rep.turt <- read.table('../data/replicate turtles fixed.txt', 
                       header = FALSE, stringsAsFactors = FALSE)
rep.turt <- df2array(rep.turt, n.land = 26, n.dim = 2)

unit <- seq(1, 50, by = 5)

gpa.turt <- list()
for(ii in seq(length(unit))) {
  sel <- unit[ii]:(unit[ii] + 4)
  gpa.turt[[ii]] <- procGPA(rep.turt[, , sel])$rotated
}

#riemdist(rep.turt[, , 1], rep.turt[, , 2])
#ssriemdist(rep.turt[, , 1], rep.turt[, , 2])  # scaled
within.dist <- list()
for(ii in seq(length(unit))) {
  confi <- gpa.turt[[ii]]
  distmat <- matrix(nrow = length(sel), ncol = length(sel))
  for(jj in seq(length(sel))) {
    for(kk in seq(length(sel))) {
      j1 <- confi[, , jj]
      k1 <- confi[, , kk]
      distmat[jj, kk] <- sum(rowSums((j1 - k1)^2))
    }
  }
  within.dist[[ii]] <- distmat
}

avg.rep <- laply(within.dist, function(x) mean(x[lower.tri(x)]))  # replicate
avg.class <- laply(class.dist, function(x) mean(x[lower.tri(x)]))  # observed
