require(ggplot2)
require(mapproj) 
require(maps)
require(ggmap)
require(reshape2)
require(xtable)
require(grid)
require(GGally)
require(scales)
require(shapes)
require(geomorph)

source('../src/support_functions.r') 
source('../src/plotting_functions.r')

load('../data/turtle_analysis.RData')
load('../data/turtle_gen.RData')

theme_set(theme_bw())

cbp <- c(#'#999999', 
         '#E69F00', '#56B4E9', '#009E73', 
         '#F0E442', '#0072B2', '#D55E00', '#CC79A7')


# map details
turtle.adult$long[which(turtle.adult$long > -100)] <- turtle.adult$long[which(turtle.adult$long > -100)] - 100

adult.train$spinks$long[which(adult.train$spinks$long > -100)] <- adult.train$spinks$long[which(adult.train$spinks$long > -100)] - 100


longma <- max(turtle.adult$long)
longmi <- min(turtle.adult$long)
latma <- max(turtle.adult$lat)
latmi <- min(turtle.adult$lat)

goog.map <- get_map(location = c(longmi,
                                 latmi, 
                                 longma, 
                                 latma), 
                    zoom = 5,
                    maptype = 'terrain',
                    color = 'bw')
ggmap(goog.map)

# PCA results
morph <- ggpairs(turtle.adult, columns = c(1:3),
                 upper = 'blank',
                 params = c(LabelSize = 2, gridLabelSize = 2, size = 1))
fits <- procGPA(turtle.land.adult)
links <- c(1:7, 13:8)
land <- mshape.plot(fits, links = links)
land <- land + coord_equal()
morph <- putPlot(morph, land, 1, 3)
pdf(file = '../documents/figure/pca_res.pdf')
print(morph)
dev.off()


# variation along first two PCs
mt <- mshape(turtle.land.adult)
pc1.max <- which.max(turtle.adult$PC1)
pc1.min <- which.min(turtle.adult$PC1)
pc2.max <- which.max(turtle.adult$PC2)
pc2.min <- which.min(turtle.adult$PC2)

ex.lab <- function(var, value) {
  value <- as.character(value)
  if(var == 'lab') {
    value[value == '1'] <- 'min'
    value[value == '2'] <- 'mean'
    value[value == '3'] <- 'max'
  }
  return(value)
}

inits <- list(turtle.land.adult[, , pc1.min], turtle.land.adult[, , pc1.max],
              mt,
              turtle.land.adult[, , pc2.min], turtle.land.adult[, , pc2.max], 
              mt)
inits <- lapply(inits, as.data.frame)
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

gpc <- ggplot(pcvar, aes(x = V2, y = -V1)) + geom_point()
gpc <- gpc + geom_segment(mapping = aes(x = V2, xend = V4,
                                        y = -V1, yend = -V3))
gpc <- gpc + facet_grid(pcl ~ pc.typ)
gpc <- gpc + theme(axis.title = element_blank(),
                   axis.text = element_blank())
ggsave(file = '../documents/figure/pc_var.png', plot = gpc)


# gap statistic plot
gap <- gap.plot(tadult.gap)
gap <- gap + theme(axis.title = element_text(size = 9.5),
                   axis.text = element_text(size = 8))
ggsave(file = '../documents/figure/gap_res.png', plot = gap)


#classes of the gap statistic plot for 2 clust
turtle.adult$long[which(turtle.adult$long > -100)] <- turtle.adult$long[which(turtle.adult$long > -100)] - 100
adult.train$spinks$long[which(adult.train$spinks$long > -100)] <- adult.train$spinks$long[which(adult.train$spinks$long > -100)] - 100

set.seed(1)
gap.second <- pam(as.dist(turtle.adult.dist), k = 2)
gap.map <- map.plot(data = turtle.adult,
                    label = gap.second$clustering,
                    map = goog.map)
gap.map <- gap.map + xlim(longmi, longma) + ylim(latmi, latma)
gap.map <- gap.map + scale_colour_manual(values = cbp)
gap.map <- gap.map + theme(legend.position = 'none',
                           legend.text = element_text(size = 7),
                           axis.title = element_text(size = 10),
                           axis.text = element_text(size = 7))
ggsave(file = '../documents/figure/gap_map.png', plot = gap.map)


# model selection plots
rf.rocs <- lapply(trf.a, function(x) {
                  rr <- x$results$ROC
                  rr})
rf.rocs <- ldply(rf.rocs)
multi.rocs <- lapply(tm.a, function(x) {
                     lapply(x, function(y) {
                            rr <- max(y$results$ROC)
                            rr})})
multi.rocs <- ldply(lapply(lapply(multi.rocs, ldply), t))
lda.rocs <- lapply(tl.a, function(x) {
                   lapply(x, function(y) {
                          y$results$ROC})})
lda.rocs <- ldply(lapply(lapply(lda.rocs, ldply), t))

names(rf.rocs) <- names(multi.rocs) 
roc.mod <- rbind(rf.rocs, multi.rocs, lda.rocs)
mod.names <- c(rep('rf', nrow(rf.rocs)), 
               rep('multi', nrow(multi.rocs)),
               rep('lda', nrow(lda.rocs)))
roc.mod <- cbind(mod.names, roc.mod)
roc.mod <- melt(roc.mod)
roc.mod$variable <- as.numeric(roc.mod$variable)

roc.mod$.id[roc.mod$.id == 'sh1'] <- 'morph 1'
roc.mod$.id[roc.mod$.id == 'sh2'] <- 'morph 2'
roc.mod$.id[roc.mod$.id == 'sh3'] <- 'molec 1'
roc.mod$.id[roc.mod$.id == 'spinks'] <- 'molec 2'

ggroc <- ggplot(roc.mod, aes(x = variable, y = value, lty = mod.names))
ggroc <- ggroc + geom_line()
ggroc <- ggroc + scale_x_continuous(breaks = seq(max(roc.mod$variable)))
ggroc <- ggroc + scale_linetype_manual(labels = c('linear discriminate analysis',
                                                  'mulitnomial logistic regression',
                                                  'random forest'),
                                      values = c(1,2,3))
ggroc <- ggroc + theme(legend.title = element_blank(),
                       #legend.position = 'bottom',
                       legend.margin = unit(0, 'cm'),
                       legend.text = element_text(size = 6),
                       axis.title = element_text(size = 10),
                       axis.text = element_text(size = 7),
                       strip.text = element_text(size = 7))
ggroc <- ggroc + labs(x = '# of features (PCs)', y = 'AUC')
ggroc <- ggroc + facet_wrap(~.id)
ggsave(file = '../documents/figure/roc_sel.png', plot = ggroc)


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
gdist <- ggplot(dd, aes(x = value, fill = X2)) + geom_density(alpha = 0.5,
                                                              size = 0.1)
gdist <- gdist + theme(#legend.position = 'bottom',
                       legend.title = element_blank(),
                       legend.margin = unit(0, 'cm'),
                       legend.text = element_text(size = 6),
                       axis.title = element_text(size = 10),
                       axis.text = element_text(size = 7))
gdist <- gdist + scale_fill_manual(name = '', values = cbp,
                                   labels = c('morph 1', 'morph 2',
                                              'molec 1', 'molec 2'))
gdist <- gdist + facet_grid(L1 ~ ., labeller = gen.name)
gdist <- gdist + labs(x = 'AUC')
ggsave(file = '../documents/figure/gen_res.png', plot = gdist)


# best model map
gg <- ggmap(goog.map)
gg <- gg + xlim(longmi, longma) + ylim(latmi, latma)
#gg <- ggplot(map, aes(x = long, y = lat, group = group))
#gg <- gg + geom_polygon(fill = 'white', colour = 'black')
#gg <- gg + coord_map('gilbert')

tr <- cbind(long = adult.test$spinks$long, 
            lat = adult.test$spinks$lat, 
            label = adult.test$spinks$spinks)
tr <- cbind(data.frame(tr), type = rep('testing', nrow(tr)))
te <- cbind(long = adult.test$spinks$long,
            lat = adult.test$spinks$lat)
mty <- c(rep('multi', nrow(te)), rep('rf', nrow(te)), rep('lda', nrow(te)))
te <- rbind(te, te, te)
te <- cbind(data.frame(te),
            label = c(tm.a.analysis$class$spinks$pred,
                      trf.a.analysis$class$spinks$pred,
                      tl.a.analysis$class$spinks$pred),
            type = mty)
turts <- rbind(tr, te)

gg <- gg + geom_point(data = turts,
                      mapping = aes(x = long, y = lat,
                                    group = NULL,
                                    colour = factor(label)))
gg <- gg + facet_wrap(~ type)
gg <- gg + scale_colour_manual(name = '', values = cbp)
gg <- gg + theme(legend.position = 'none',
                 legend.margin = unit(0, 'cm'),
                 legend.text = element_text(size = 6),
                 axis.title = element_text(size = 10),
                 axis.text = element_text(size = 7))
ggsave(file = '../documents/figure/gen_map.png', plot = gg)


# 3 most important variables
most.imp <- trf.a$spinks$optVariables
ww <- 'spinks'
turtle.adult$class[turtle.adult[, ww] == 1] = 'Northern'
turtle.adult$class[turtle.adult[, ww] == 2] = 'Eastern'
turtle.adult$class[turtle.adult[, ww] == 3] = 'Western'
turtle.adult$class[turtle.adult[, ww] == 4] = 'Southern'
turtle.adult$class <- factor(turtle.adult$class, 
                             levels = c('Northern', 
                                        'Eastern', 
                                        'Western', 
                                        'Southern'))

ggimp <- ggpairs(turtle.adult,
                 columns = c(most.imp[1:3], 'class'), 
                 colour = 'class',
                 upper = 'blank',
                 lower = 'blank',
                 params = c(LabelSize = 2, gridLabelSize = 2, size = 1))
pc1 <- ggplot(turtle.adult, mapping = aes(x = PC3, y = PC2, colour = class))
pc1 <- pc1 + geom_point() + scale_color_manual(values = cbp)
pc2 <- ggplot(turtle.adult, mapping = aes(x = PC3, y = PC1, colour = class))
pc2 <- pc2 + geom_point() + scale_color_manual(values = cbp)
pc3 <- ggplot(turtle.adult, mapping = aes(x = PC2, y = PC1, colour = class))
pc3 <- pc3 + geom_point() + scale_color_manual(values = cbp)
hist1 <- ggplot(turtle.adult, mapping = aes(x = PC3, fill = class))
hist1 <- hist1 + geom_histogram()
hist1 <- hist1 + facet_grid(class ~ .) + scale_fill_manual(values = cbp)
hist2 <- ggplot(turtle.adult, mapping = aes(x = PC2, fill = class))
hist2 <- hist2 + geom_histogram()
hist2 <- hist2 + facet_grid(class ~ .) + scale_fill_manual(values = cbp)
hist3 <- ggplot(turtle.adult, mapping = aes(x = PC1, fill = class))
hist3 <- hist3 + geom_histogram()
hist3 <- hist3 + facet_grid(class ~ .) + scale_fill_manual(values = cbp)

ggimp <- putPlot(ggimp, pc1, 2, 1)
ggimp <- putPlot(ggimp, pc2, 3, 1)
ggimp <- putPlot(ggimp, pc3, 3, 2)
ggimp <- putPlot(ggimp, hist1, 4, 1)
ggimp <- putPlot(ggimp, hist2, 4, 2)
ggimp <- putPlot(ggimp, hist3, 4, 3)
pdf(file = '../documents/figure/pca_imp.pdf')
print(ggimp)
dev.off()
#ggsave(plot = ggimp)


# mean of the different classes
# these are going to be combined into a single plot using latex
mt <- mshape(turtle.land.adult)
mts <- mt[, 2:1]
mts[, 2] <- -1 * mts[, 2]
spi <- turtle.adult$spinks
wspi <- lapply(levels(spi), function(x, y) which(y == x), y = spi)
mspi <- lapply(wspi, function(x, y) mshape(y[, , x]), y = turtle.land.adult)
mins <- lapply(mspi, function(x) min(x[, 2]))
mins <- min(unlist(mins))
lmat <- cbind(links, c(links[-1], links[1]))
mspi <- lapply(mspi, function(x) x[, 2:1])
mspi <- lapply(mspi, function(x) {
               x[, 2] <- -1 * x[, 2]
               x})
for(ii in seq(length(mspi))) {
  pdf(file = paste0('../documents/figure/mshape_', ii, '.pdf'))
  plotRefToTarget(M1 = mts, M2 = mspi[[ii]], mag = 2, links = lmat)
  dev.off()
}


# variation along most important axes
fst.max <- which.max(turtle.adult[, most.imp[1]])
fst.min <- which.min(turtle.adult[, most.imp[1]])
snd.max <- which.max(turtle.adult[, most.imp[2]])
snd.min <- which.min(turtle.adult[, most.imp[2]])

ex.lab <- function(var, value) {
  value <- as.character(value)
  if(var == 'lab') {
    value[value == '1'] <- 'min'
    value[value == '2'] <- 'mean'
    value[value == '3'] <- 'max'
  }
  return(value)
}

comps <- list(turtle.land.adult[, , fst.min], turtle.land.adult[, , fst.max],
              mt,
              turtle.land.adult[, , snd.min], turtle.land.adult[, , snd.max], 
              mt)
comps <- lapply(comps, as.data.frame)
comps <- lapply(comps, function(x) x[links, ])
comps <- lapply(comps, function(x) cbind(x, rbind(x[-1, ], x[1, ])))
comps <- lapply(comps, function(x) {
                names(x) <- c('V1', 'V2', 'V3', 'V4')
                x})
comps <- Reduce(rbind, comps)
shl <- unlist(lapply(most.imp[1:2], function(x) rep(x, 3 * nrow(mt))))
shl <- factor(shl, levels = c('PC3', 'PC2'))
typ <- c(rep('min', nrow(mt)), rep('max', nrow(mt)))
mm <- rep('mean', nrow(mt))
typ <- c(typ, mm, typ, mm)
varshape <- cbind(comps, shl, typ)

gsh <- ggplot(varshape, aes(x = V2, y = -V1)) + geom_point()
gsh <- gsh + geom_segment(mapping = aes(x = V2, xend = V4,
                                        y = -V1, yend = -V3))
gsh <- gsh + facet_grid(shl ~ typ)
gsh <- gsh + theme(axis.title = element_blank(),
                   axis.text = element_blank())
ggsave(file = '../documents/figure/imp_var.png', plot = gsh)


# relative risk of the multinomial logistic regression model
relrisk <- t.a.rr$spinks
relci <- t.a.rr.ci$spinks
op <- trf.a$spinks$optVariables
rel <- list()
for(ii in seq(nrow(relrisk))) {
  rel[[ii]] <- as.data.frame(cbind(relrisk[ii, op], relci[op, , ii]))
  colnames(rel[[ii]]) <- c('rr', 'bot', 'up')
}
names(rel) <- c('eastern', 'western', 'southern')
rel <- lapply(rel, function(x) cbind(x, pc = rownames(x)))
rel <- mapply(function(x, y) cbind(x, class = rep(y, nrow(x))), 
              x = rel, y = names(rel),
              SIMPLIFY = FALSE)
rel <- Reduce(rbind, rel)

rel$pc <- factor(rel$pc, levels = trf.a$spinks$optVariables)
rel <- rel[rel$pc %in% op[1:3], ]

ggrel <- ggplot(rel, aes(x = class, y = rr, ymax = up, ymin = bot)) 
ggrel <- ggrel + geom_pointrange()
ggrel <- ggrel + geom_hline(aes(yintercept = 0), lty = 2)
ggrel <- ggrel + facet_wrap(~ pc)
ggrel <- ggrel + labs(y = 'relative risk')
ggrel <- ggrel + theme(axis.text = element_text(size = 9)) 

ggsave(file = '../documents/figure/rel_risk.png', plot = ggrel)


# plot of the linear discriminate analysis
lda.scal <- tl.a.analysis$best$spinks$finalModel$scaling
lda.points <- as.data.frame(as.matrix(adult.train$spinks[, 1:10]) %*% lda.scal)
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
pdf(file = '../documents/figure/lda.pdf')
print(gglda)
dev.off()
