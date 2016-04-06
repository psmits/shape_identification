library(shapes)
library(geomorph)
source('../R/df2array.r')
source('../R/supervised_mung.r')

by.scheme <- list()
scheme <- c('sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks')

rep.turt <- read.table('../data/replicate turtles fixed.txt', 
                       header = FALSE, stringsAsFactors = FALSE)
rep.turt <- df2array(rep.turt, n.land = 26, n.dim = 2)

combined <- abind(land.adult, rep.turt)
combo.turt <- procGPA(combined)

adult$sh1

unit <- rep(c('r1', 'r2', 'r3', 'r4', 'r5', 'r6', 'r7', 'r8', 'r9', 'r0'), 
            each = 5)
types <- c(scheme, unique(unit))

for(jj in seq(6)) {
  by.class <- list()
  sch <- as.character(adult[, scheme[jj]])
  sch <- c(sch, unit)
  sch.t <- unique(sch)
  for(ii in seq(length(sch.t))) {
    sch.w <- sch == sch.t[ii]
    by.class[[ii]] <- combo.turt$rotated[, , sch.w]
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

sch.md <- rep.md <- c()
sch.dd <- rep.dd <- list()
for(ii in seq(length(by.scheme))) {
  sch <- as.character(adult[, scheme[ii]])
  sch.t <- length(unique(sch))
  
  sch.dist <- class.dist[[ii]][seq(sch.t), seq(sch.t)]
  sch.dd[[ii]] <- sch.dist
  sch.md[ii] <- mean(sch.dist[lower.tri(sch.dist)])
  
  ee <- nrow(class.dist[[ii]])
  ss <- ee - sch.t
  rep.dist <- class.dist[[ii]][seq(ss + 1, ee), seq(ss+1, ee)]
  rep.dd[[ii]] <- rep.dist
  rep.md[ii] <- mean(rep.dist[lower.tri(rep.dist)])
}
