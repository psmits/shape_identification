###############################################################################
##
##  plotting and tables
##
##  ben frable
##  peter d smits
##
###############################################################################

## libraries
require(reshape2)
require(grid)
require(ggplot2)
require(GGally)
require(xtable)

## source files
source('../src/support_functions.r')
source('../src/fish_analysis.r')


## tiny bit of munging
frames <- lapply(fits, function(x) land.frame(x$rotated))
frames$fish <- cbind(frames$fish,
                     c(rep('ano', dim(frames$ano)[1]), 
                       rep('cur', dim(frames$cur)[1])))
names(frames$fish)[3] <- 'group'


## tables



## mean shapes
means <- lapply(fits, function(x) as.data.frame(x$mshape))
means <- lapply(means, function(x) cbind(x, rbind(x[-1, ], x[1, ])))
means <- lapply(means, function(x) {
                names(x) <- c('V1', 'V2', 'V3', 'V4'); return(x)
                     })
gmean <- function(x, m) {
  gm <- ggplot(x, aes(x = x, y = y)) + geom_point()
  gm <- gm + geom_point(data = m, 
                        mapping = aes(x = V1, 
                                      y = V2, 
                                      colour = 'red', 
                                      size = 1.5))
  gm <- gm + geom_segment(data = m,
                          mapping = aes(x = V1,
                                        xend = V3,
                                        y = V2,
                                        yend = V4,
                                        size = 1.2))
  gm <- gm + theme(#panel.grid = element_blank(), 
                   panel.background = element_blank(), 
                   legend.position = 'none',
                   axis.line = element_line(colour = 'black'))
  gm
}
gg.means <- Map(gmean, frames, means)


## PCA plots
scores <- lapply(fits, function(x) as.data.frame(x$stdscores))
fish.names <- list(rep('ano', dim(ano.land)[3]),
                   rep('cur', dim(cur.land)[3]),
                   rep('pro', dim(pro.land)[3]),
                   rep('chi', dim(chi.land)[3]),
                   fish.group)
scores <- Map(cbind, scores, fish.names)
scores <- lapply(scores, function(x) {names(x)[ncol(x)] <- 'group'; return(x)})
gg.pca <- lapply(scores, function(x) ggpairs(x,
                                             columns = c(1:4, ncol(x)),
                                             upper = 'blank',
                                             colour = 'group'))


## machine learning results
## best.fish.mod
## x is eigenscore
## y is probability
## facet plot where facets are PCs
## line of probability for each category
fish.melt <- reshape2::melt(cbind(rownames(fish.test), fish.test))
fish.probs.melt <- reshape2::melt(cbind(rownames(fish.pred.prob), fish.pred.prob))
fish.melts <- fish.probs.melts <- data.frame()
for (ii in seq(length(unique(fish.probs.melt$variable)))) {
  fish.melts <- rbind(fish.melts, fish.melt)
}
colnames(fish.melts) <- c('id', 'real', 'PC', 'eigenscore')

fish.probs.melt <- split(fish.probs.melt, f = fish.probs.melt$variable)
fish.probs.melts <- fish.probs.melt
for (jj in seq(length(unique(fish.melt$variable)) - 1)) {
  ## this isn't working correctly
  ## need to split and then combine with likes
  ## currently fucks up the plot
  fish.probs.melts <- Map(rbind, fish.probs.melts, fish.probs.melt)
}
colnames(fish.probs.melts) <- c('id', 'estimate', 'probability')
fish.super.melt <- cbind(fish.melts, fish.probs.melts)
fish.super.melt <- fish.super.melt[, -5]
fish.super.melt <- fish.super.melt[(fish.super.melt$PC == 'PC1' |
                                   fish.super.melt$PC == 'PC2'), ]

gf <- ggplot(fish.super.melt, aes(x = eigenscore, y = probability, colour = estimate))
gf <- gf + geom_line()
gf <- gf + facet_wrap(~ PC, scales = 'free')
## we have problems....not getting probabilities for all the variables at the same time

## cluster plots
## do these in ggplot because it is a much nicer framework for this kind of analysis
#ggclust <- lapply(clust, function(x) lapply(x, as.hclust))

