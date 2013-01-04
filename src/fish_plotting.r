###############################################################################
##
##  plotting and tables
##
##  ben frable
##  peter d smits
##
###############################################################################

## libraries
require(shapes)
require(geomorph)
require(reshape2)
require(grid)
require(ggplot2)
require(GGally)
require(ggdendro)
require(xtable)

## source files
source('support_functions.r')
source('fish_analysis.r')


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
                   fish.group)
scores <- Map(cbind, scores, fish.names)
scores <- lapply(scores, function(x) {names(x)[ncol(x)] <- 'group'; return(x)})
gg.pca <- lapply(scores, function(x) ggpairs(x,
                                             columns = 1:4,
                                             upper = 'blank',
                                             colour = 'group'))


## discriminate analysis
## this may not be necessary because of the quality of my PCA plots...

## cluster plots
## do these in ggplot because it is a much nicer framework for this kind of analysis
ggclust <- lapply(clust, function(x) lapply(x, as.hclust))

