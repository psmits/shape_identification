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
require(ggplot2)
require(grid)
require(xtable)

## source files
source('support_functions.r')
source('fish_analysis.r')

## tables


## mean shapes
frames <- lapply(fits, function(x) land.frame(x$rotated))
frames$fish <- cbind(frames$fish,
                     c(rep('ano', dim(frames$ano)[1]), 
                       rep('cur', dim(frames$cur)[1])))
names(frames$fish)[3] <- 'group'
means <- lapply(fits, function(x) as.data.frame(x$mshape))
gmean <- function(x, m) {
  gm <- ggplot(x, aes(x = x, y = y)) + geom_point()
  gm <- gm + geom_point(data = m, 
                        mapping = aes(x = V1, 
                                      y = V2, 
                                      colour = 'red', 
                                      size = 1.5))
  gm <- gm + theme(#panel.grid = element_blank(), 
                   panel.background = element_blank(), 
                   legend.position = 'none',
                   axis.line = element_line(colour = 'black'))
  gm
}
gg.means <- Map(gmean, frames, means)


## PCA plots


## cluster plots
