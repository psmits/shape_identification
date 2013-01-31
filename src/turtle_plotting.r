###############################################################################
##
##  plotting and tables for the turtle data
##
##  peter d smits
##  psmits@uchicago.edu
##
###############################################################################

require(ggplot2)
require(reshape2)
require(plyr)
require(GGally)
require(maps)
require(mapproj)


source('../src/turtle_analysis.r')

# shape PCA
gtmorph <- ggpairs(turtle.info, columns = c(1:5, 36), 
                   upper = 'blank', 
                   colour = 'spinks')

# clustering results
# gap
tgeo.gap.d <- as.data.frame(tgeo.gap$Tab)
tgeo.gap.d <- cbind(id = rownames(tgeo.gap.d), tgeo.gap.d)
gtgeo.gap <- ggplot(tgeo.gap.d, aes(x = id, y = gap))
gtgeo.gap <- gtgeo.gap + geom_pointrange(mapping = aes(ymax = gap + SE.sim,
                                                       ymin = gap - SE.sim))
gtgeo.gap <- gtgeo.gap + labs(x = 'Number of clusters',
                              y = 'Gap statistic')

tmorph.gap.d <- as.data.frame(tmorph.gap$Tab)
tmorph.gap.d <- cbind(id = rownames(tmorph.gap.d), tmorph.gap.d)
gtmorph.gap <- ggplot(tmorph.gap.d, aes(x = id, y = gap))
gtmorph.gap <- gtmorph.gap + geom_pointrange(mapping = aes(ymax = gap + SE.sim,
                                                           ymin = gap - SE.sim))
gtmorph.gap <- gtmorph.gap + labs(x = 'Number of clusters',
                                  y = 'Gap statistic')

# map
st <- map_data('state')
gs <- c('california', 'oregon', 'washington')
california <- st[st$region == gs, ]
#california$long <- abs(california$long)
tgeo <- as.data.frame(turtle.geo)
tgeo$V2 <- tgeo$V2 * -1
tgeo <- cbind(tgeo, turtle.geo.meta, fuzzy = tgeo.fuzzy$clustering)
gtgeo <- ggplot(california, aes(x = long, y = lat, group = group))
gtgeo <- gtgeo + geom_polygon(fill = 'white', 
                              colour = 'black')
gtgeo <- gtgeo + coord_map('gilbert')
gtgeo.spinks <- gtgeo + geom_point(data = tgeo,
                                   mapping = aes(x = V2, y = V1,
                                                 group = NULL,
                                                 colour = factor(spinks)))
gtgeo.sh1 <- gtgeo + geom_point(data = tgeo,
                                mapping = aes(x = V2, y = V1,
                                              group = NULL,
                                              colour = sh1))
gtgeo.sh2 <- gtgeo + geom_point(data = tgeo,
                                mapping = aes(x = V2, y = V1,
                                              group = NULL,
                                              colour = sh2))
gtgeo.sh3 <- gtgeo + geom_point(data = tgeo,
                                mapping = aes(x = V2, y = V1,
                                              group = NULL,
                                              colour = sh3))
# fuzzy clustering results of geography
gtgeo.fuzzy <- gtgeo + geom_point(data = tgeo,
                                  mapping = aes(x = V2, V1,
                                                group = NULL,
                                                colour = factor(fuzzy)))
#gtgeo <- gtgeo + theme(legend.position = 'none')
