library(shapes)
library(gridExtra)
library(MuMIn)
library(geomorph)
library(plyr)
library(stringr)
library(reshape2)
library(xtable)
library(boot)
library(ggplot2)
library(grid)
library(scales)
# source files
source('../R/array2df.r')
source('../R/df2array.r')
source('../R/align_plot.r')
#
theme_set(theme_bw())
cbp <- c('#000000', '#E69F00', '#56B4E9', '#009E73', 
         '#F0E442', '#0072B2', '#D55E00', '#CC79A7')
cbp.long <- c('#000000', '#004949', '#009292', '#FF7DB6', '#FFB6DB', 
              '#490092', '#006DDB', '#B66DFF', '#6DB6FF', '#B6DBFF', 
              '#920000', '#924900', '#DBD100', '#24FF24', '#FFFF6D')

grab <- laply(seq(5), function(x) seq(from = x, to = length(cbp.long), by = 5))
cbp.ord <- cbp.long[t(grab)]


# actually start doing analysis...
newturt <- list.files('../data/new_turtle', 
                      pattern = 'adult', 
                      full.names = TRUE)
turt <- llply(newturt, function(x) read.csv(x, header = FALSE))
numbers <- llply(turt, function(x) x[, 1:2])
centroids <- llply(turt, function(x) x[, ncol(x)])
turt <- llply(turt, function(x) x[, -c(1:2, ncol(x))])
# number, museum #, lands...., centroid
turt <- Reduce(rbind, turt)
turt.align <- df2array(turt, n.land = 26, n.dim = 2)
turt.proc <- procGPA(turt.align)
turt.scores <- turt.proc$scores

seven.centroids <- unlist(centroids)
turt.name <- laply(str_split(newturt, '\\/'), function(x) x[length(x)])
turt.name <- str_trim(str_extract(turt.name, '\\s(.*?)\\s'))



trac <- list.files('../data/trach', pattern = 'txt', full.names = TRUE)
trac <- llply(trac, function(x) 
              read.table(x, header = FALSE, stringsAsFactors = FALSE))
# lands...., centroid
trac.centroids <- unlist(llply(trac, function(x) x[, ncol(x)]))
ids <- Reduce(c, Map(function(x, y) 
                     rep(y, times = nrow(x)), 
                     x = trac, y = c('a', 'b')))
trac <- llply(trac, function(x) x[, -(ncol(x))])
trac <- Reduce(rbind, trac)
trac.align <- df2array(trac, n.land = 26, n.dim = 2)
trac.proc <- procGPA(trac.align)
trac.scores <- trac.proc$scores



scores.df <- data.frame(rbind(turt.proc$stdscores,
                              trac.proc$stdscores))
scores.df$species <- c(rep(turt.name, times = laply(numbers, nrow)),
                       ids)
scores.df$who <- c(rep('7 species', turt.proc$n), 
                   rep('Trachemys', trac.proc$n))
scores.df$centroid <- c(seven.centroids, trac.centroids)
# turt.proc$percent[1:2]  # percent of variation on PC

clear.gg <- ggplot(scores.df, aes(x = PC1, y = PC2, 
                                  colour = species, size = log10(centroid)))
clear.gg <- clear.gg + geom_point()
clear.gg <- clear.gg + facet_grid(. ~ who)
clear.gg <- clear.gg + labs(x = 'PC 1', y = 'PC 2')
clear.gg <- clear.gg + scale_colour_manual(values = cbp.ord)
ggsave(plot = clear.gg, filename = '../doc/figure/other_pc_graph.png',
       width = 6, height = 4)

xs <- split(scores.df, f = scores.df$who)
p1 <- ggplot(xs$cc7, aes(x = PC1, y = PC2, 
                         colour = sp, size = log10(centroid)))
p1 <- p1 + geom_point()
p1 <- p1 + facet_grid(. ~ who)
p2 <- p1 %+% xs$trac
#AlignPlots(p1,p2)
#grid.arrange(p1,p2)



# now for marmorata data
source('../R/supervised_mung.r')
schemes <- c('sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks')
adult$spinks <- LETTERS[adult$spinks]

ad <- data.frame(adult[, schemes], stringsAsFactors = FALSE)
level <- unique(unlist(apply(adult[, schemes], 2, 
                             function(x) levels(as.factor(x)))))


ad <- apply(ad, 2, as.character)
ad <- Reduce(c, ad)  # vector of the classifications for all schemes


sch <- rep(schemes, each = nrow(adult))  # vector of the schemes for ad
sch <- mapvalues(sch, from = schemes, to = c('Morph 1', 'Morph 2', 'Mito 1',
                                             'Nuclear', 'Mito 2', 'Mito 3'))

scores <- Reduce(rbind, 
                 replicate(length(schemes), fit$stdscores, simplify = FALSE))

scores.df <- data.frame(scores)
scores.df$class <- factor(ad, levels = level)
scores.df$sch <- sch
#fit$percent[1:2]
scores.df$centroid <- rep(rawturt[, n.land + 1], length(schemes))

emys.gg <- ggplot(scores.df, aes(x = PC1, y = PC2, 
                                 colour = class, size = log10(centroid)))
emys.gg <- emys.gg + geom_point(alpha = 0.3)
emys.gg <- emys.gg + facet_wrap(~ sch, nrow = 2)
emys.gg <- emys.gg + coord_fixed(ratio = 1, xlim = c(-4, 4), ylim = c(-4, 4))
emys.gg <- emys.gg + scale_colour_manual(values = cbp.ord)
emys.gg <- emys.gg + labs(x = 'PC 1 (32.1%)', y = 'PC 2 (15.2%)')
ggsave(plot = emys.gg, filename = '../doc/figure/emys_pc_graph.png',
       width = 8, height = 6)
