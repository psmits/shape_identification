library(shapes)
library(reshape2)
library(stringr)
library(geomorph)
library(ggplot2)
library(scales)
library(grid)
source('../R/df2array.r')
source('../R/supervised_mung.r')
source('../R/multiplot.r')

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

dfg <- data.frame(null = null)


theme_set(theme_minimal())
dfgp <- ggplot(dfg, aes(x = null))
dfgp <- dfgp + geom_histogram(colour = 'darkgrey', fill = 'lightgrey')
dfgp <- dfgp + geom_vline(xintercept = sex.mean, size = 2)
dfgp <- dfgp + scale_x_continuous(breaks = pretty_breaks(4))
dfgp <- dfgp + labs(x = 'Procrustes distance', y = 'Frequency')
ggsave(plot = dfgp, filename = '../doc/figure/sex_test_hist.png',
       width = 4, height = 3)


# by groups
gg <- c('sp10.1', 'sp10.2', 'sp10.3', 'sp14.1', 'sp14.2', 'morph')
ggl <- list()
for(jj in seq(length(gg))) {
  tt <- adult[, c(gg[jj], 'p.sex')]
  tt <- tt[!is.na(tt[, 1]), ]
  tt[, 2] <- as.character(tt[, 2])
  fe <- grep(pattern = '[fF]', tt[, 2], perl = TRUE)
  ma <- grep(pattern = '[mM]', tt[, 2], perl = TRUE)
  na <- grep(pattern = '[nN]', tt[, 2], perl = TRUE)
  tt[fe, 2] <- 'F'
  tt[ma, 2] <- 'M'

  tt <- tt[tt[, 1] != '', ]
  sexk <- which(tt[, 2] %in% c('F', 'M'))
  tsex <- tt[sexk, 2]
  print(table(interaction(tsex, tt[sexk, 1])))
  sex.shape <- fit$rotated[, , (sexk)]


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

  dfg <- data.frame(null = null)

  print(sum(null > sex.mean) / length(null))

  theme_set(theme_minimal())
  dfgp <- ggplot(dfg, aes(x = null))
  dfgp <- dfgp + geom_histogram(colour = 'darkgrey', fill = 'lightgrey')
  dfgp <- dfgp + geom_vline(xintercept = sex.mean, size = 2)
  dfgp <- dfgp + scale_x_continuous(breaks = pretty_breaks(4))
  dfgp <- dfgp + labs(title = gg[jj],
                      x = 'Procrustes distance', y = 'Frequency')
  name <- paste0('../doc/figure/sex_test_hist_', gg[jj], '.png')
  ggsave(plot = dfgp, filename = name, width = 4, height = 3)
  ggl[[jj]] <- dfgp
}
png(filename = '../doc/figure/sex_test_hist_grouped.png', width = 880, height = 880)
multiplot(plotlist = ggl, cols = 2)
dev.off()
