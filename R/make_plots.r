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


# rf variable importance
rf.imp <- llply(for.imp, function(x) x$importance)
rf.imp <- Map(function(x, y) data.frame(cbind(x[, (ncol(x) - 1):(ncol(x))], 
                                              gr = rep(y, nrow(x)))), 
                                        x = rf.imp, 
                                        y = seq(length(rf.imp)))
rf.imp <- llply(rf.imp, function(x) {
                x$pc <- rownames(x)
                x})
rf.imp <- data.frame(Reduce(rbind, rf.imp))
colnames(rf.imp)[1:2] <- c('acc', 'gini')
rf.imp$pc <- factor(rf.imp$pc, levels = rev(unique(rf.imp$pc)))
grf <- ggplot(rf.imp, aes(x = gini, y = pc))
grf <- grf + geom_point(size = 5)
#grf <- grf + geom_bar(stat = 'identity', width = 0.5)
grf <- grf + facet_grid(. ~ gr)
grf <- grf + labs(x = 'Mean decrease\nin Gini Index', 
                  y = 'Principal Component')
grf <- grf + theme(axis.title = element_text(size = 20),
                   axis.title.y = element_text())
ggsave(file = '../doc/figure/var_imp.png', plot = grf,
       width = 15, height = 5)

# selection accumulation curves
m.aic <- llply(tm.a.analysis$aic, function(x) {
               x <- data.frame(cbind(x, n = seq(length(x))))
               x})
min.m <- cbind(x = laply(m.aic, function(x) min(x$x)), y = groups)
min.m <- cbind(min.m, n = laply(m.aic, function(x) which.min(x$x)))
m.aic <- Map(function(x, y) cbind(x, gr = rep(y, nrow(x))),
             x = m.aic, y = groups)
m.aic <- Reduce(rbind, m.aic)
m.aic <- cbind(m.aic, ty = rep('aic', nrow(m.aic)))

l.auc <- llply(tl.a.analysis$auc, function(x) {
               x <- data.frame(cbind(x, n = seq(length(x))))
               x})
max.l <- cbind(x = laply(l.auc, function(x) max(x)), y = groups)
max.l <- cbind(max.l, n = laply(l.auc, function(x) which.max(x$x)))
l.auc <- Map(function(x, y) cbind(x, gr = rep(y, nrow(x))),
             x = l.auc, y = groups)
l.auc <- Reduce(rbind, l.auc)
l.auc <- cbind(l.auc, ty = rep('auc', nrow(l.auc)))

maxes <- data.frame(rbind(cbind(min.m, ty = rep('aic', nrow(min.m))), 
                          cbind(max.l, ty = rep('auc', nrow(max.l)))))
names(maxes)[2] <- 'gr'
maxes <- data.frame(apply(maxes, 2, unlist))
maxes$x <- as.numeric(as.character(maxes$x))
maxes$n <- as.numeric(as.character(maxes$n))

sel <- rbind(m.aic, l.auc)

gsel <- ggplot(sel, aes(x = n, y = x))
gsel <- gsel + geom_line(size = 1.5) + geom_point(size = 3)
gsel <- gsel + facet_grid(ty ~ gr, scales = 'free')
gsel <- gsel + geom_point(data = maxes, 
                          mapping = aes(x = n, y = x), colour = 'red',
                          size = 5)
gsel <- gsel + labs(x = 'Cummulative number of\nprincipal components', 
                    y = 'Value')
ggsave(file = '../doc/figure/sel_val.png', plot = gsel,
       width = 15, height = 10)



# generalize plot
gen.name <- function(var, value) {
  value <- as.character(value)
  if(var == 'L1') {
    value[value == 'mn'] <- 'multinomial logistic regression'
    value[value == 'rf'] <- 'random forest'
    value[value == 'lda'] <- 'linear discriminate analysis'
    value[value == 'lrf'] <- 'selected LDA'
  }
  return(value)
}

oo <- Reduce(cbind, Map(function(x) x$t, rr))
colnames(oo) <- groups
uu <- Reduce(cbind, Map(function(x) x$t, mm))
colnames(uu) <- groups
vv <- Reduce(cbind, Map(function(x) x$t, ll))
colnames(vv) <- groups
lr <- Reduce(cbind, Map(function(x) x$t, lrf))
colnames(lr) <- groups
zz <- list(rf = oo, mn = uu, lda = vv, lrf = lr)
dd <- melt(zz)
gdist <- ggplot(dd, aes(x = value, fill = Var2))
gdist <- gdist + geom_histogram(aes(y = ..density..),
                                alpha = 0.3,
                                size = 0.1,
                                position = 'identity',
                                colour = 'darkgrey',
                                binwidth = 1/100)
gdist <- gdist + scale_fill_manual(name = '', values = cbp,
                                   labels = c('morph 1', 'morph 2',
                                              'molec 1', 'two species',
                                              '2014', 'molec 2'))
gdist <- gdist + facet_grid(L1 ~ ., labeller = gen.name)
gdist <- gdist + labs(x = 'AUC')
gdist <- gdist + coord_cartesian(xlim = c(0, 1))
gdist <- gdist + theme(legend.title = element_blank(),
                       legend.margin = unit(0, 'cm'),
                       legend.text = element_text(size = 6))
ggsave(file = '../doc/figure/gen_res.png', plot = gdist,
       width = 15, height = 10)


# plot the results of the generalizations on a map
names(tl.a.analysis$class) <- groups
gen.maps <- lapply(groups, function(x) {
                   pred.map(map = ggmap(goog.map), 
                            xl = c(longmi, longma+1),
                            yl = c(latmi-2, latma),
                            test = adult.test,
                            name = x,
                            mods = list(tm.a.analysis, 
                                        trf.a.analysis, 
                                        tl.a.analysis,
                                        gr),
                            types = c('multi', 'rf', 'lda', 'lrf'),
                            data = adult)})
names(gen.maps) <- groups
gen.maps <- lapply(gen.maps, function(x) {
                   x + scale_colour_manual(name = '', values = cbp) + 
                   scale_fill_manual(name = '', values = cbp)})
for(ii in seq(length(gen.maps))) {
  ggsave(file = paste('../doc/figure/gen_map', ii, '.png', sep = ''),
         plot = gen.maps[[ii]],
         height = 11, width = 8.5)
}


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


# mean of the different classes
# these are going to be combined into a single plot using latex
mspi <- lapply(pp, function(x) {
               class.mean(x, land.adult, adult)})
mt <- mshape(land.adult)
mts <- mt[, 2:1]
mts[, 2] <- -1 * mts[, 2]
lmat <- cbind(links, c(links[-1], links[1]))
lmat <- rbind(lmat, matrix(snd.links, nrow = 5, byrow = TRUE))
for(jj in seq(length(mspi))) {
  for(ii in seq(length(mspi[[jj]]))) {
    pdf(file = paste0('../doc/figure/mshape_', pp[jj], '_', ii, '.pdf'), 
        width = 3.4)
    par(mar = c(0, 0, 0, 0), xaxs = 'i')
    plotRefToTarget(M1 = mts, M2 = mspi[[jj]][[ii]], mag = 2, links = lmat)
    dev.off()
  }
}


# variation along most important axes
gsh <- lapply(most.imp, function(x) shape.imp(x, adult, land.adult, links, snd.links))
for(ii in seq(length(gsh))) {
  ggsave(file = paste0('../doc/figure/imp_var_', groups[[ii]], '.png'), 
         plot = gsh[[ii]], height = 15, width = 10)
}
