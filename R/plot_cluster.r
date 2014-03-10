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

source('../R/plotting_functions.r')

source_url('https://raw.github.com/JoFrhwld/FAAV/master/r/stat-ellipse.R')

load('../data/shape.RData')

theme_set(theme_bw())

cbp <- c(#'#999999', 
         '#E69F00', '#56B4E9', '#009E73', 
         '#F0E442', '#0072B2', '#D55E00', '#CC79A7')

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


# correlation between the first two axes and centroid size
bcs <- c(cs, cs)
cent <- cbind(cs = bcs, pc = c(adult$PC1, adult$PC2))
cent <- cbind(as.data.frame(cent),
              lab = c(rep('PC1', length(adult$PC1)),
                      rep('PC2', length(adult$PC2))))
gcs <- ggplot(cent, aes(x = cs, y = pc)) 
gcs <- gcs + geom_point() + stat_smooth(method = 'lm')
gcs <- gcs + facet_wrap(~lab)
gcs <- gcs + labs(x = 'centroid size', y = 'eigenscore')
ggsave(file = '../documents/figure/cent_size.png', plot = gcs)


# gap statistic plot
gap <- gap.plot(tadult.gap)
gap <- gap + theme(axis.title = element_text(size = 9.5),
                   axis.text = element_text(size = 8))
ggsave(file = '../documents/figure/gap_res.png', plot = gap)


#classes of the gap statistic plot for 2 clust
adult$long[which(adult$long > -100)] <- adult$long[which(adult$long > -100)] - 100
adult.train$spinks$long[which(adult.train$spinks$long > -100)] <- adult.train$spinks$long[which(adult.train$spinks$long > -100)] - 100

set.seed(1)
gap.second <- pam(as.dist(adult.dist), k = 2)
gap.map <- map.plot(data = adult,
                    label = gap.second$clustering,
                    map = goog.map)
gap.map <- gap.map + xlim(longmi - 1, longma) + ylim(latmi, latma)
gap.map <- gap.map + scale_shape_manual(values = c(1,2))
gap.map <- gap.map + stat_ellipse(geom = 'polygon',
                                  data = adult,
                                  mapping = aes(x = long,
                                                y = lat,
                                                fill = spinks),
                                  alpha = 0.35)
gap.map <- gap.map + scale_fill_manual(values = cbp)
gap.map <- gap.map + theme(legend.position = 'none',
                           legend.text = element_text(size = 7),
                           axis.title = element_text(size = 10),
                           axis.text = element_text(size = 7))
ggsave(file = '../documents/figure/gap_map.png', plot = gap.map)



