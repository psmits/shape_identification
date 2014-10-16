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
library(plyr)

source('../R/plotting_functions.r')

source_url('https://raw.github.com/JoFrhwld/FAAV/master/r/stat-ellipse.R')

load('../data/gen.RData')

theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')
theme_update(axis.text = element_text(size = 20),
             axis.title = element_text(size = 30),
             axis.title.y = element_text(hjust = 0.1),
             legend.text = element_text(size = 25),
             legend.title = element_text(size = 26),
             legend.key.size = unit(2, 'cm'),
             strip.text = element_text(size = 25))

# map details
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


# PCA results
morph <- ggpairs(adult, columns = c(1:3),
                 upper = 'blank',
                 params = c(LabelSize = 2, gridLabelSize = 2, size = 1))
fits <- procGPA(land.adult)
links <- c(1:7, 13:8)
snd.links <- c(2, 8, 3, 9, 4, 10, 5, 11, 6, 12)
land <- mshape.plot(fits, links = links, snd.links = snd.links)
land <- land + coord_equal()
morph <- putPlot(morph, land, 1, 3)
pdf(file = '../doc/figure/pca_res.pdf')
print(morph)
dev.off()


# variation along first two PCs
mt <- mshape(land.adult)
pc1.max <- which.max(adult$PC1)
pc1.min <- which.min(adult$PC1)
pc2.max <- which.max(adult$PC2)
pc2.min <- which.min(adult$PC2)

ex.lab <- function(var, value) {
  value <- as.character(value)
  if(var == 'lab') {
    value[value == '1'] <- 'min'
    value[value == '2'] <- 'mean'
    value[value == '3'] <- 'max'
  }
  return(value)
}

inits <- list(land.adult[, , pc1.min], land.adult[, , pc1.max],
              mt,
              land.adult[, , pc2.min], land.adult[, , pc2.max], 
              mt)
secs <- inits <- lapply(inits, as.data.frame)
inits <- lapply(inits, function(x) x[links, ])
inits <- lapply(inits, function(x) cbind(x, rbind(x[-1, ], x[1, ])))
inits <- lapply(inits, function(x) {
                names(x) <- c('V1', 'V2', 'V3', 'V4')
                x})
inits <- Reduce(rbind, inits)
pcl <- unlist(lapply(c('PC1', 'PC2'), function(x) rep(x, 3 * nrow(mt))))
pc.typ <- c(rep('min', nrow(mt)), rep('max', nrow(mt)))
pc.mm <- rep('mean', nrow(mt))
pc.typ <- c(pc.typ, pc.mm, pc.typ, pc.mm)
pcvar <- cbind(inits, pcl, pc.typ)

# create the coordinates for the second set of links
secs.l <- lapply(secs, function(x) x[snd.links, ])
ss <- seq(from = 1, to = length(snd.links), by = 2)
secs.l <- lapply(secs.l, function(x) cbind(x[ss, ], x[ss + 1, ]))
secs.l <- lapply(secs.l, function(x) {
                 names(x) <- c('V1', 'V2', 'V3', 'V4')
                 x})
secs.l <- Reduce(rbind, secs.l)
pcl <- unlist(lapply(c('PC1', 'PC2'), 
                        function(x) rep(x, 3 * length(snd.links) / 2)))
pc.typ <- c(rep('min', length(snd.links) / 2),
             rep('max', length(snd.links) / 2))
pc.mm <- rep('mean', length(snd.links) / 2)
pc.typ <- c(pc.typ, pc.mm, pc.typ, pc.mm)
secs.l <- cbind(secs.l, pcl, pc.typ)


gpc <- ggplot(pcvar, aes(x = V2, y = -V1)) + geom_point()
gpc <- gpc + geom_segment(mapping = aes(x = V2, xend = V4,
                                        y = -V1, yend = -V3))
# add in the secondary links
for(ii in seq(from = 1, to = nrow(secs.l), by = 2)) {
  gpc <- gpc + geom_segment(data = secs.l[c(ii, ii + 1), ],
                            mapping = aes(x = V2,
                                          xend = V4,
                                          y = -V1,
                                          yend = -V3))
}
gpc <- gpc + facet_grid(pcl ~ pc.typ)
gpc <- gpc + theme(axis.title = element_blank(),
                   axis.text = element_blank())
ggsave(file = '../doc/figure/pc_var.png', plot = gpc,
       width = 15, height = 10)


## model selection plots
#rf.rocs <- lapply(trf.a, function(x) {
#                  rr <- x$results$ROC
#                  rr})
#rf.rocs <- ldply(rf.rocs)
#multi.rocs <- lapply(tm.a, function(x) {
#                     lapply(x, function(y) {
#                            rr <- max(y$results$ROC)
#                            rr})})
##multi.rocs <- ldply(lapply(lapply(multi.rocs, ldply), t))
#lda.rocs <- lapply(tl.a, function(x) {
#                   lapply(x, function(y) {
#                          y$results$ROC})})
#lda.rocs <- ldply(lapply(lapply(lda.rocs, ldply), t))
#
#names(rf.rocs) <- names(multi.rocs) 
#roc.mod <- rbind(rf.rocs, multi.rocs, lda.rocs)
#mod.names <- c(rep('rf', nrow(rf.rocs)), 
#               rep('multi', nrow(multi.rocs)),
#               rep('lda', nrow(lda.rocs)))
#roc.mod <- cbind(mod.names, roc.mod)
#roc.mod <- melt(roc.mod)
#roc.mod$variable <- as.numeric(roc.mod$variable)
#roc.mod$.id[roc.mod$.id == 'sh1'] <- 'morph 1'
#roc.mod$.id[roc.mod$.id == 'sh2'] <- 'morph 2'
#roc.mod$.id[roc.mod$.id == 'sh3'] <- 'molec 1'
#roc.mod$.id[roc.mod$.id == 'sh4'] <- 'two species'
#roc.mod$.id[roc.mod$.id == 'sh5'] <- '2014'
#roc.mod$.id[roc.mod$.id == 'spinks'] <- 'molec 2'
#
#ggroc <- ggplot(roc.mod, aes(x = variable, y = value, lty = mod.names))
#ggroc <- ggroc + geom_line()
#ggroc <- ggroc + scale_x_continuous(breaks = seq(max(roc.mod$variable)))
#ggroc <- ggroc + scale_linetype_manual(labels = c('linear discriminate analysis',
#                                                  'mulitnomial logistic regression',
#                                                  'random forest'),
#                                       values = c(1,2,3))
#ggroc <- ggroc + theme(legend.title = element_blank(),
#                       #legend.position = 'bottom',
#                       legend.margin = unit(0, 'cm'),
#                       legend.text = element_text(size = 6),
#                       axis.title = element_text(size = 10),
#                       axis.text = element_text(size = 7),
#                       strip.text = element_text(size = 7))
#ggroc <- ggroc + labs(x = '# of features (PCs)', y = 'AUC')
#ggroc <- ggroc + facet_wrap(~.id)
#ggsave(file = '../doc/figure/roc_sel.png', plot = ggroc)


# generalize plot
gen.name <- function(var, value) {
  value <- as.character(value)
  if(var == 'L1') {
    value[value == 'mn'] <- 'multinomial logistic regression'
    value[value == 'rf'] <- 'random forest'
    value[value == 'lda'] <- 'linear discriminate analysis'
  }
  return(value)
}

oo <- Reduce(cbind, Map(function(x) x$t, rr))
colnames(oo) <- groups
uu <- Reduce(cbind, Map(function(x) x$t, mm))
colnames(uu) <- groups
vv <- Reduce(cbind, Map(function(x) x$t, ll))
colnames(vv) <- groups
zz <- list(rf = oo, mn = uu, lda = vv)
dd <- melt(zz)
gdist <- ggplot(dd, aes(x = value, fill = Var2))
gdist <- gdist + geom_histogram(alpha = 0.3,
                                size = 0.1,
                                position = 'identity',
                                colour = 'darkgrey')
gdist <- gdist + scale_fill_manual(name = '', values = cbp,
                                   labels = c('morph 1', 'morph 2',
                                              'molec 1', 'two species',
                                              '2014', 'molec 2'))
gdist <- gdist + facet_grid(L1 ~ ., labeller = gen.name)
gdist <- gdist + labs(x = 'AUC')
gdist <- gdist + theme(legend.title = element_blank(),
                       legend.margin = unit(0, 'cm'),
                       legend.text = element_text(size = 6))
ggsave(file = '../doc/figure/gen_res.png', plot = gdist,
       width = 15, height = 10)


## plot the results of the generalizations on a map
#gen.maps <- lapply(groups, function(x) {
#                   pred.map(map = ggmap(goog.map), 
#                            xl = c(longmi, longma), yl = c(latmi, latma),
#                            test = adult.test, name = x,
#                            mods = list(tm.a.analysis, 
#                                        trf.a.analysis, 
#                                        tl.a.analysis),
#                            types = c('multi', 'rf', 'lda'), data = adult)})
#names(gen.maps) <- groups
#for(ii in seq(length(gen.maps))) {
#  ggsave(file = paste('../doc/figure/gen_map', ii, '.png', sep = ''),
#         plot = gen.maps[[ii]],
#         height = 11, width = 8.5)
#}


# 3 most important variables
# this needs to be updated to reflect most important
ww <- 'spinks'
adult$class[adult[, ww] == 1] = 'Northern'
adult$class[adult[, ww] == 2] = 'Eastern'
adult$class[adult[, ww] == 3] = 'Western'
adult$class[adult[, ww] == 4] = 'Southern'
adult$class <- factor(adult$class, 
                      levels = c('Northern', 
                                 'Eastern', 
                                 'Western', 
                                 'Southern'))

pp <- c('sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks')
most.imp <- lapply(trf.a, function(x) x$optVariables)
imp.plots <- list()
for(ii in seq(length(pp))) {
  if(length(most.imp[[ii]]) < 2) {
    many <- 3
  } else {
    many <- 2
  }
  imp.plots[[ii]] <- imp.pairs(most.imp[[ii]], many, pp[ii], adult, cbp)
}
for(ii in seq(length(imp.plots))) {
  pdf(file = paste('../doc/figure/pca_imp', ii, '.pdf', sep = ''))
  print(imp.plots[[ii]])
  dev.off()
}


## mean of the different classes
## these are going to be combined into a single plot using latex
#mspi <- lapply(pp, function(x) {
#               class.mean(x, land.adult, adult)})
#mt <- mshape(land.adult)
#mts <- mt[, 2:1]
#mts[, 2] <- -1 * mts[, 2]
#for(jj in seq(length(mspi))) {
#  for(ii in seq(length(mspi[[jj]]))) {
#    pdf(file = paste0('../doc/figure/mshape_', pp[jj], '_', ii, '.pdf'), 
#        width = 3.4)
#    par(mar = c(0, 0, 0, 0), xaxs = 'i')
#    plotRefToTarget(M1 = mts, M2 = mspi[[jj]][[ii]], mag = 2, links = lmat)
#    dev.off()
#  }
#}


# variation along most important axes
gsh <- lapply(most.imp, function(x) shape.imp(x, adult, land.adult, links, snd.links))
for(ii in seq(length(gsh))) {
  ggsave(file = paste0('../doc/figure/imp_var_', groups[[ii]], '.png'), 
         plot = gsh[[ii]], height = 15, width = 10)
}


# relative risk of the multinomial logistic regression model
relrisk <- t.a.rr$spinks
relci <- t.a.rr.ci$spinks
op <- trf.a$spinks$optVariables
#relrisk.plot(relrisk, relci, op, 2)
#ggsave(file = '../documents/figure/rel_risk.png', plot = ggrel)


# plot of the linear discriminate analysis
# this needs to be generalized
lda.scal <- tl.a.analysis$best$spinks$scaling
lda.points <- as.data.frame(as.matrix(adult.train$spinks[, 1:4]) %*% lda.scal)
lda.points <- cbind(lda.points,
                    class = adult.train$spinks$spinks)
lda.points$class <- as.character(lda.points$class)
lda.points$class[lda.points$class == 1] = 'Northern'
lda.points$class[lda.points$class == 2] = 'Eastern'
lda.points$class[lda.points$class == 3] = 'Western'
lda.points$class[lda.points$class == 4] = 'Southern'
lda.points$class <- factor(lda.points$class,
                           levels = c('Northern',
                                      'Eastern',
                                      'Western',
                                      'Southern'))

gglda <- ggpairs(lda.points, 
                 colour = 'class',
                 upper = 'blank',
                 lower = 'blank',
                 params = c(LabelSize = 2, gridLabelSize = 2, size = 1))
ld1 <- ggplot(lda.points, mapping = aes(x = LD1, y = LD2, colour = class))
ld1 <- ld1 + geom_point() + scale_color_manual(values = cbp)
ld2 <- ggplot(lda.points, mapping = aes(x = LD1, y = LD3, colour = class))
ld2 <- ld2 + geom_point() + scale_color_manual(values = cbp)
ld3 <- ggplot(lda.points, mapping = aes(x = LD2, y = LD3, colour = class))
ld3 <- ld3 + geom_point() + scale_color_manual(values = cbp)
lh1 <- ggplot(lda.points, mapping = aes(x = LD1, fill = class))
lh1 <- lh1 + geom_histogram()
lh1 <- lh1 + facet_grid(class ~ .) + scale_fill_manual(values = cbp)
lh2 <- ggplot(lda.points, mapping = aes(x = LD2, fill = class))
lh2 <- lh2 + geom_histogram()
lh2 <- lh2 + facet_grid(class ~ .) + scale_fill_manual(values = cbp)
lh3 <- ggplot(lda.points, mapping = aes(x = LD3, fill = class))
lh3 <- lh3 + geom_histogram()
lh3 <- lh3 + facet_grid(class ~ .) + scale_fill_manual(values = cbp)
gglda <- putPlot(gglda, ld1, 2, 1)
gglda <- putPlot(gglda, ld2, 3, 1)
gglda <- putPlot(gglda, ld3, 3, 2)
gglda <- putPlot(gglda, lh1, 4, 1)
gglda <- putPlot(gglda, lh2, 4, 2)
gglda <- putPlot(gglda, lh3, 4, 3)
pdf(file = '../docs/figure/lda.pdf')
print(gglda)
dev.off()
