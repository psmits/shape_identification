library(shapes)
library(reshape2)
library(stringr)
library(geomorph)
source('../R/df2array.r')
source('../R/supervised_mung.r')

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


tsex <- as.character(adult$p.sex)  # lots of regex to get this sexy
fe <- grep(pattern = '[fF]', tsex, perl = TRUE)
ma <- grep(pattern = '[mM]', tsex, perl = TRUE)
na <- grep(pattern = '[nN]', tsex, perl = TRUE)
tsex[fe] <- 'F'
tsex[ma] <- 'M'
tsex[na] <- NA
tsex[tsex == ''] <- NA

sexrm <- which(is.na(tsex))

tsex <- tsex[-sexrm]
sex.shape <- fit$rotated[, , -(sexrm)]


# reference
sex.split <- split.land(sex.shape, tsex)
sex.mean <- llply(sex.split, mshape)
sex.mean <- sum(rowSums((sex.mean[[1]] - sex.mean[[2]])^2))

null <- c()
for(ii in seq(1000)) {
  tt <- sample(tsex)
  ss <- split.land(sex.shape, tt)
  sm <- llply(ss, mshape)
  null[ii] <- sum(rowSums((sm[[1]] - sm[[2]])^2))
}


