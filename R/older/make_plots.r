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

#source_url('https://raw.github.com/JoFrhwld/FAAV/master/r/stat-ellipse.R') # 

load('../data/gen.RData')

theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')
theme_update(axis.text = element_text(size = 20),
             axis.title = element_text(size = 30),
             legend.text = element_text(size = 25),
             legend.title = element_text(size = 26),
             legend.key.size = unit(2, 'cm'),
             strip.text = element_text(size = 20))


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
rf.imp$gr <- mapvalues(rf.imp$gr, unique(rf.imp$gr), 
                       c('Morph 1', 'Morph 2', 'Mito 1', 
                         'Nuclear', 'Mito 2', 'Mito 3'))

grf <- ggplot(rf.imp, aes(x = gini, y = pc))
grf <- grf + geom_point(size = 2)
#grf <- grf + geom_bar(stat = 'identity', width = 0.5)
grf <- grf + facet_grid(. ~ gr)
grf <- grf + labs(x = 'Mean decrease\nin Gini Index', 
                  y = 'Principal Component')
grf <- grf + theme(axis.title = element_text(size = 20),
                   axis.title.y = element_text())
ggsave(file = '../doc/figure/var_imp.png', plot = grf,
       width = 8, height = 10)

# selection accumulation curves
m.auc <- llply(tm.a.analysis$auc, function(x) {
               x <- data.frame(cbind(x, n = seq(length(x))))
               x})
max.m <- cbind(x = laply(m.auc, function(x) max(x$x)), y = groups)
max.m <- cbind(max.m, n = laply(m.auc, function(x) which.max(x$x)))
m.auc <- Map(function(x, y) cbind(x, gr = rep(y, nrow(x))),
             x = m.auc, y = groups)
m.auc <- Reduce(rbind, m.auc)
m.auc <- cbind(m.auc, ty = rep('multinomial logistic regression', nrow(m.auc)))

l.auc <- llply(tl.a.analysis$auc, function(x) {
               x <- data.frame(cbind(x, n = seq(length(x))))
               x})
max.l <- cbind(x = laply(l.auc, function(x) max(x[, 1])), y = groups)
max.l <- cbind(max.l, n = laply(l.auc, function(x) which.max(x$x)))
l.auc <- Map(function(x, y) cbind(x, gr = rep(y, nrow(x))),
             x = l.auc, y = groups)
l.auc <- Reduce(rbind, l.auc)
l.auc <- cbind(l.auc, ty = rep('linear discriminiate analysis', nrow(l.auc)))

maxes <- data.frame(rbind(cbind(max.m, 
                                ty = rep('multinomial logistic regression', 
                                         nrow(max.m))), 
                          cbind(max.l, 
                                ty = rep('linear discriminiate analysis', 
                                         nrow(max.l)))))
names(maxes)[2] <- 'gr'
maxes <- data.frame(apply(maxes, 2, unlist))
maxes$x <- as.numeric(as.character(maxes$x))
maxes$n <- as.numeric(as.character(maxes$n))

sel <- rbind(m.auc, l.auc)
sel$gr <- as.character(sel$gr)
sel$gr <- mapvalues(sel$gr, unique(sel$gr), 
                    c('Morph 1', 'Morph 2', 'Mito 1', 
                      'Nuclear', 'Mito 2', 'Mito 3'))
maxes$gr <- as.character(maxes$gr)
maxes$gr <- mapvalues(maxes$gr, unique(maxes$gr), 
                      c('Morph 1', 'Morph 2', 'Mito 1', 
                        'Nuclear', 'Mito 2', 'Mito 3'))


gsel <- ggplot(sel, aes(x = n, y = x))
gsel <- gsel + geom_line(size = 1.5) + geom_point(size = 3)
gsel <- gsel + facet_grid(ty ~ gr, scales = 'free')
gsel <- gsel + geom_point(data = maxes, 
                          mapping = aes(x = n, y = x), shape = 1,
                          size = 8)
gsel <- gsel + labs(x = 'Cummulative number of\nprincipal components', 
                    y = 'AUC')
gsel <- gsel + coord_cartesian(ylim = c(0.5, 1))
ggsave(file = '../doc/figure/sel_val.png', plot = gsel,
       width = 10, height = 8)



# generalize plot
gen.name <- function(value) {
  value <- as.character(value)
  value[value == 'mn'] <- 'MLR'
  value[value == 'rf'] <- 'RF'
  value[value == 'lda'] <- 'LDA'
  value[value == 'lrf'] <- 'selected LDA'
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
names(dd)[3] <- 'val'

dd$Var2 <- as.character(dd$Var2)
dd$Var2 <- mapvalues(dd$Var2, unique(dd$Var2), 
                     c('Morph 1', 'Morph 2', 'Mito 1', 'Nuclear', 'Mito 2', 'Mito 3'))

gdist <- ggplot(dd, aes(x = val))
gdist <- gdist + geom_histogram(#alpha = 0.3,
                                size = 0.1,
                                position = 'identity',
                                #colour = 'darkgrey',
                                binwidth = 1/100)
gdist <- gdist + scale_fill_manual(name = '', values = cbp,
                                   labels = c('Morph 1', 'Morph 2',
                                              'Mito 1', 'Nuclear',
                                              'Mito 2', 'Mito 3'))
gdist <- gdist + facet_grid(Var2 ~ L1, labeller = labeller(L1 = gen.name))
gdist <- gdist + labs(x = 'AUC', y = 'Count')
#gdist <- gdist + coord_cartesian(xlim = c(0, 1))
gdist <- gdist + theme(legend.title = element_blank(),
                       legend.margin = unit(0, 'cm'),
                       legend.text = element_text(size = 6))
ggsave(file = '../doc/figure/gen_res.png', plot = gdist,
       width = 15, height = 10)
