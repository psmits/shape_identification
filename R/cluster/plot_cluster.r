library(ggplot2)
library(mapproj) 
library(maps)
library(ggmap)
library(reshape2)
library(xtable)
library(grid)
library(GGally)
library(scales)
library(shapes)
library(geomorph)
library(devtools)
library(cluster)

source('../R/plotting_functions.r')
source('../R/supervised_mung.r')

source_url('https://raw.github.com/JoFrhwld/FAAV/master/r/stat-ellipse.R')

load('../data/cluster_res.RData')

theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')

adult$long[which(adult$long > -100)] <- adult$long[which(adult$long > -100)] - 100
adult.train$spinks$long[which(adult.train$spinks$long > -100)] <- adult.train$spinks$long[which(adult.train$spinks$long > -100)] - 100

longma <- max(adult$long)
longmi <- min(adult$long)
latma <- max(adult$lat)
latmi <- min(adult$lat)

goog.map <- get_map(location = c(longmi,
                                 latmi, 
                                 longma, 
                                 latma), 
                    zoom = 5,
                    maptype = 'terrain',
                    color = 'bw')
ggmap(goog.map)


# gap statistic plot
gap <- gap.plot(tadult.gap)
gap <- gap + theme(axis.text = element_text(size = 20),
                   axis.title = element_text(size = 30),
                   legend.text = element_text(size = 25),
                   legend.title = element_text(size = 26),
                   legend.key.size = unit(2, 'cm'),
                   strip.text = element_text(size = 25))
ggsave(file = '../doc/figure/gap_res.png', plot = gap,
       width = 15, height = 10)



#classes of the gap statistic plot for 2 clust
adult$long[which(adult$long > -100)] <- adult$long[which(adult$long > -100)] - 100
adult.train$spinks$long[which(adult.train$spinks$long > -100)] <- adult.train$spinks$long[which(adult.train$spinks$long > -100)] - 100

set.seed(1)
gap.second <- pam(as.dist(adult.dist), k = 2)
data <- adult
label <- gap.second$clustering
map <- goog.map
gap.map <- map.plot(data = adult,
                    label = gap.second$clustering,
                    map = goog.map)
gap.map <- gap.map + xlim(longmi - 1, longma) + ylim(latmi, latma)
gap.map <- gap.map + scale_shape_manual(values = c(15,16))
gap.map <- gap.map + labs(x = 'longitude', y = 'latitude')
gap.map <- gap.map + theme(legend.position = 'none',
                           axis.text = element_text(size = 20),
                           axis.title = element_text(size = 30),
                           legend.text = element_text(size = 25),
                           legend.title = element_text(size = 26),
                           legend.key.size = unit(2, 'cm'),
                           strip.text = element_text(size = 25))
ggsave(file = '../doc/figure/gap_map.png', plot = gap.map,
       width = 15, height = 10)
