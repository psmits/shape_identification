###############################################################################
##
##  miscellaneous morphometric data sets included in the packages
##  plotting results of various analyses
##
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
source('../src/misc_data_analysis.r')


## tables


## procrustes fit results
land.scores <- lapply(land.proc, function(x) as.data.frame(x$stdscore))
land.scores <- Map(cbind, land.scores,
                   list(mon.species,
                        mouse.group))
land.scores <- lapply(land.scores, 
                      function(x) {names(x)[ncol(x)] <- 'group'; return(x)})
land.pca <- lapply(land.scores, 
                   function(x) ggpairs(x,
                                       columns = c(1:4, ncol(x)),
                                       upper = 'blank',
                                       colour = 'group'))


## machine learning results
