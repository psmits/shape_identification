library(shapes)
library(stringr)
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
# should i re-split the normal lands from the rep.turts?

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



# between species distance

newturt <- list.files('../data/new_turtle', 
                      pattern = 'adult', 
                      full.names = TRUE)
turt <- llply(newturt, function(x) read.csv(x, header = FALSE))
numbers <- llply(turt, function(x) x[, 1:2])
centroids <- llply(turt, function(x) x[, ncol(x)])
turt <- llply(turt, function(x) x[, -c(1:2, ncol(x))])
# number, museum #, lands...., centroid
turt <- Reduce(rbind, turt)
turt.align <- df2array(turt, n.land = 26, n.dim = 2)
turt.proc <- procGPA(turt.align)
turt.scores <- turt.proc$scores

centroids <- scale(unlist(centroids))
turt.name <- laply(str_split(newturt, '\\/'), function(x) x[length(x)])
turt.name <- str_trim(str_extract(turt.name, '\\s(.*?)\\s'))
#rep(turt.name, times = laply(numbers, nrow))


by.species <- list()
groups <- laply(numbers, nrow)
group.mod <- cumsum(groups)
for(ii in seq(length(turt.name))) {
  if(ii == 1) {
    ss <- seq(from = 1, group.mod[ii])
  } else {
    ss <- seq(from = group.mod[ii - 1] + 1, to = group.mod[ii])
  }
  by.species[[ii]] <- turt.proc$rotated[, , ss]
}


species.mean <- llply(by.species, mshape)
distmat <- matrix(nrow = length(by.species),
                  ncol = length(by.species))
for(jj in seq(length(species.mean))) {
  for(kk in seq(length(species.mean))) {
    distmat[jj, kk] <- sum(rowSums((species.mean[[jj]] - species.mean[[kk]])^2))
  }
}
mean.between.speciesmeans <- mean(distmat[lower.tri(distmat)])
