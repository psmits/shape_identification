library(shapes)
library(geomorph)

library(plyr)
library(stringr)
library(reshape2)

library(xtable)
library(grid)
library(gridExtra)
library(ggplot2)
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
                      pattern = 'txt', 
                      full.names = TRUE)
turt <- llply(newturt, function(x) read.delim(x, header = FALSE, sep = ' '))
# for some reason there are 2 dead columns...
turt <- llply(turt, function(x) { 
                x = x[1:27]
                x})

# need to get rid of the JRB specimens
inturt <- list.files('../data/new_turtle', 
                      pattern = 'list.csv', 
                      full.names = TRUE)
# blan, coa, gut, ins, muh, orb, orn, pic
#inturt <- inturt[c(1, 3, 4, 5, 6, 7, 8, 2)]
numbers <- llply(inturt, function(x) read.csv(x, header = TRUE))

# remove JRB before things get awkward
spec.source <- llply(numbers, function(x) as.character(x[, 2]))
to.rm <- llply(spec.source, function(x) str_detect(x, 'JRB'))

turt <- Map(function(x, y) {x = x[!y, ]; x}, turt, to.rm)
numbers <- Map(function(x, y) {x = x[!y, ]; x}, numbers, to.rm)

# ok, onto the analysis
centroids <- llply(turt, function(x) x[, ncol(x)])
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
                     x = trac, y = c('T. scripta elegans', 'T. scripta scripta')))
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

splits <- split(scores.df, scores.df$who)

# turt.proc$percent[1:2]  # percent of variation on PC

splits[[1]]$g <- 'cc7'
splits[[2]]$g <- 'tra'

#splits <- dplyr::bind_rows(splits[[1]], splits[[2]], .id = 'g')


clear.gg <- ggplot(splits[[1]], aes(x = PC1, y = PC2, 
                                    colour = species, size = centroid))
clear.gg <- clear.gg + geom_point(alpha = 0.5)
clear.gg <- clear.gg + scale_size_area(max_size = 3)
clear.gg <- clear.gg + scale_colour_manual(values = cbp.ord)
#clear.gg <- clear.gg + facet_wrap(~ g)

cc7g <- clear.gg + labs(x = 'PC 1 (41.6%)', 
                        y = 'PC 2 (19.1%)', 
                        title = 'A.')
cc7g <- cc7g + coord_fixed(ratio = 1)
trag <- clear.gg %+% splits[[2]] + labs(x = 'PC 1 (37.9%)', 
                                        y = 'PC 2 (17.3%)', 
                                        title = 'B.')
trag <- trag + coord_fixed(ratio = 1)

png(filename = '../doc/figure/other_pc_graph.png',
    width = 850, height = 450)
grid.arrange(cc7g, trag, ncol = 2)
dev.off()


ggsave(plot = cc7g, filename = '../doc/figure/cc7_pc_graph.png',
       width = 8, height = 6)
ggsave(plot = trag, filename = '../doc/figure/tra_pc_graph.png',
       width = 8, height = 6)



# now for marmorata data
source('../R/supervised_mung.r')
schemes <- c('sp10.1', 'sp10.2', 'sp10.3', 'sp14.1', 'sp14.2', 'morph')

ad <- data.frame(adult[, schemes], stringsAsFactors = FALSE)
ad <- apply(ad, 2, as.character)
level <- unique(unlist(apply(adult[, schemes], 2, 
                             function(x) levels(as.factor(x)))))

ad <- apply(ad, 2, as.character)
ad <- Reduce(c, ad)  # vector of the classifications for all schemes


sch <- rep(schemes, each = nrow(adult))  # vector of the schemes for ad

scores <- Reduce(rbind, 
                 replicate(length(schemes), fit$stdscores, simplify = FALSE))

scores.df <- data.frame(scores)
scores.df$class <- factor(ad, levels = level)
scores.df$sch <- sch
scores.df$centroid <- rep(rawturt[, n.land + 1], length(schemes))

# need to be made human readable
scores.df$class <- as.character(scores.df$class)
scores.df <- scores.df[!is.na(scores.df$class), ]
scores.df <- scores.df[scores.df$class != '', ]
#unique(scores.df$class)
scores.df$class <- mapvalues(scores.df$class, 
                             from = unique(scores.df$class), 
                             to = c('Central Coast', 
                                    'E. marmorata', 'E. pallida', 
                                    'San Juan', 'Baja California', 
                                    'Sierra Foothills'))

perc <- fit$percent[1:2]
perc <- round(perc, digits = 1)
emys.gg <- ggplot(scores.df, aes(x = PC1, y = PC2, 
                                 colour = class, size = centroid))
emys.gg <- emys.gg + geom_point(alpha = 0.5)
emys.gg <- emys.gg + scale_size_area(max_size = 4)
emys.gg <- emys.gg + facet_wrap(~ sch, nrow = 2)
emys.gg <- emys.gg + coord_fixed(ratio = 1, 
                                 xlim = c(-3.5, 3.5), 
                                 ylim = c(-3.5, 3.5))
emys.gg <- emys.gg + scale_colour_manual(values = cbp.ord)
emys.gg <- emys.gg + labs(x = paste0('PC 1 (', perc[1], '%)'), 
                          y = paste0('PC 2 (', perc[2], '%)'))
ggsave(plot = emys.gg, filename = '../doc/figure/emys_pc_graph.png',
       width = 8, height = 6)
