library(pacman)

p_load(here, shapes, reshape2, stringr, geomorph, tidyverse)

source(here::here('R', '01_supervised_mung.r'))
source(here::here('R', 'helper04_df2array.r'))


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


results_table <- list()



# emys set
scheme <- c('sp10.1', 'sp10.2', 'sp10.3', 'sp14.1', 'sp14.2', 'morph')
ff <- adult[, scheme]
emys.split <- list()
for(ii in seq(length(scheme))) {
  rms <- is.na(ff[, ii]) | ff[, ii] == ''
  temp <- ff[!rms, ii]
  tempfit <- fit$rotated[, , !rms]
  emys.split[[ii]] <- split.land(tempfit, temp)
}
emys.dist <- land.dist(fit$rotated)
within.scheme <- llply(emys.split, function(x) llply(x, land.dist))
within.scheme <- llply(within.scheme, function(x) 
                       laply(x, function(y) mean(y[lower.tri(y)])))


scheme.mean <- llply(emys.split, function(x) llply(x, mshape))
between.scheme <- list()
for(kk in seq(length(scheme.mean))) {
  bs <- length(scheme.mean[[kk]])
  distmat <- matrix(nrow = bs, ncol = bs)
  for(ii in seq(length(scheme.mean[[kk]]))) {
    for(jj in seq(length(scheme.mean[[kk]]))) {
      distmat[ii, jj] <- sum(rowSums((scheme.mean[[kk]][[ii]] - 
                                      scheme.mean[[kk]][[jj]])^2))
    }
  }
  between.scheme[[kk]] <- mean(distmat[lower.tri(distmat)])
}


scheme.ratio <- laply(within.scheme, function(x) 
                      mean(x) / mean(emys.dist[lower.tri(emys.dist)]))

# record
results_table$emys <- scheme.ratio
names(results_table$emys) <- scheme






# replicated turtles

rep.turt <- read.table(here::here('data', 'replicate_turtles_fixed.txt'), 
                       header = FALSE, stringsAsFactors = FALSE)
rep.turt <- df2array(rep.turt, n.land = 26, n.dim = 2)
unit <- rep(c('r1', 'r2', 'r3', 'r4', 'r5', 'r6', 'r7', 'r8', 'r9', 'r0'), 
            each = 5)
rep.turt <- procGPA(rep.turt)

rep.split <- split.land(rep.turt$rotated, unit)
rep.dist <- land.dist(rep.turt$rotated)
within.rep <- llply(rep.split, land.dist)
within.rep <- laply(within.rep, function(x) mean(x[lower.tri(x)]))

rep.mean <- llply(rep.split, mshape)
bs <- length(rep.mean)
distmat <- matrix(nrow = bs, ncol = bs)
for(ii in seq(length(rep.mean))) {
  for(jj in seq(length(rep.mean))) {
    distmat[ii, jj] <- sum(rowSums((rep.mean[[ii]] - rep.mean[[jj]])^2))
  }
}
between.rep <- mean(distmat[lower.tri(distmat)])


rep.ratio <- mean(within.rep) / mean(rep.dist[lower.tri(rep.dist)])

# record
results_table$replicate <- rep.ratio






# between species distance
newturt <- list.files(here::here('data', 'new_turtle'), 
                      pattern = 'fixed', 
                      full.names = TRUE)
turt <- llply(newturt, function(x) 
              read.delim(x, header = FALSE, sep = ' ')[, 1:27])
numbers <- list.files(here::here('data', 'new_turtle'), 
                      pattern = 'list.csv', 
                      full.names = TRUE)
numbers <- llply(numbers, function(x) read.csv(x, header = TRUE))
numbers <- llply(numbers, function(x) x[, 1:2])
centroids <- llply(turt, function(x) x[, ncol(x)])
turt <- llply(turt, function(x) x[, -(ncol(x))])
# number, museum #, lands...., centroid
turt <- Reduce(rbind, turt)
turt.align <- df2array(turt, n.land = 26, n.dim = 2)
turt.proc <- procGPA(turt.align)

turt.name <- laply(str_split(newturt, '\\/'), function(x) x[length(x)])
turt.name <- str_trim(str_extract(turt.name, '\\s(.*?)\\s'))
turt.name <- rep(turt.name, times = laply(numbers, nrow))

turt.split <- split.land(turt.proc$rotated, turt.name)
turt.dist <- land.dist(turt.proc$rotated)
within.species <- llply(turt.split, land.dist)
within.species <- laply(within.species, function(x) mean(x[lower.tri(x)]))


species.mean <- llply(turt.split, mshape)
bs <- length(species.mean)
distmat <- matrix(nrow = bs, ncol = bs)
for(ii in seq(length(species.mean))) {
  for(jj in seq(length(species.mean))) {
    distmat[ii, jj] <- sum(rowSums((species.mean[[ii]] - species.mean[[jj]])^2))
  }
}
between.species <- mean(distmat[lower.tri(distmat)])


species.ratio <- mean(within.species) / mean(turt.dist[lower.tri(turt.dist)])

# record
results_table$seven <- species.ratio


# between species distance (trach)
trac <- list.files(here::here('data', 'trach'), 
                   pattern = 'txt', full.names = TRUE)
turt <- llply(trac, function(x) 
              read.table(x, header = FALSE, stringsAsFactors = FALSE))
# lands...., centroid
centroids <- scale(unlist(llply(turt, function(x) x[, ncol(x)])))
ids <- Reduce(c, Map(function(x, y) 
                     rep(y, times = nrow(x)), 
                     x = turt, y = c('a', 'b')))
turt <- llply(turt, function(x) x[, -(ncol(x))])
turt <- Reduce(rbind, turt)
turt.align <- df2array(turt, n.land = 26, n.dim = 2)
turt.proc <- procGPA(turt.align)

turt.split <- split.land(turt.proc$rotated, ids)
turt.dist <- land.dist(turt.proc$rotated)
within.species <- llply(turt.split, land.dist)
within.species <- laply(within.species, function(x) mean(x[lower.tri(x)]))


species.mean <- llply(turt.split, mshape)
bs <- length(species.mean)
distmat <- matrix(nrow = bs, ncol = bs)
for(ii in seq(length(species.mean))) {
  for(jj in seq(length(species.mean))) {
    distmat[ii, jj] <- sum(rowSums((species.mean[[ii]] - species.mean[[jj]])^2))
  }
}
between.species <- mean(distmat[lower.tri(distmat)])


species.ratio2 <- mean(within.species) / mean(turt.dist[lower.tri(turt.dist)])

# record
results_table$trach <- species.ratio2


out <- flatten(results_table) %>%
melt(results_table)
names(out) <- c('ratio', 'scheme')

out$corrected <- out$ratio * results_table$replicate


write_csv(out,
          path = here::here('doc', 'digitization_error.csv'))



# ratio of
#   average within group distance / average between individuals distance
#   1+ = grouping is counter-intuitive
#   1 = no grouping
#   0 = amazing grouping
# scheme.ratio
# rep.ratio
# species.ratio
# species.ratio2

