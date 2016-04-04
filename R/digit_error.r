library(shapes)
library(geomorph)
source('../R/df2array.r')
source('../R/supervised_mung.r')

procdist <- function(x, y) {
  dis <- c()
  for(ii in seq(dim(y)[3])) {
    dif <- x - y[, , ii]  # diff of x coord, y coord
    dif <- dif^2
    dis[ii] <- sum(rowSums(dif))
  }
  dis
}


by.scheme <- list()
scheme <- c('sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks')
for(jj in seq(6)) {
  by.class <- list()
  sch <- as.character(adult[, scheme[ii]])
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
  class.dist[[ii]] <- Map(function(x, y) procdist(x, y), 
                          class.mean, by.scheme[[ii]])
}
# have the proc dist from mean shape for every class in every scheme



rep.turt <- read.table('../data/replicate turtles fixed.txt', 
                       header = FALSE, stringsAsFactors = FALSE)
rep.turt <- df2array(rep.turt, n.land = 26, n.dim = 2)
gpa.turt <- procGPA(rep.turt)

#riemdist(rep.turt[, , 1], rep.turt[, , 2])
#ssriemdist(rep.turt[, , 1], rep.turt[, , 2])  # scaled


unit <- seq(1, 50, by = 5)
within.dist <- list()
for(ii in seq(length(unit))) {
  within.dist[[ii]] <- procdist(gpa.turt$rotated[, , unit[ii]], 
                         gpa.turt$rotated[, , (unit[ii] + 1):(unit[ii] + 4)])
}


