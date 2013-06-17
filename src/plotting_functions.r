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
require(ggmap)

source('../src/support_functions.r')

gap.plot <- function(gap) {
  # wrapper to make a plot of the gap statistic results from cluster
  #
  # Args:
  #   gap: table of results from clusGap
  #
  # Returns:
  #   object of class ggplot

  tab <- as.data.frame(gap$Tab)
  tab <- cbind(id = as.numeric(rownames(tab)), tab)
  gg <- ggplot(tab, aes(x = id, y = gap))
  gg <- gg + geom_pointrange(mapping = aes(ymax = gap + SE.sim,
                                           ymin = gap - SE.sim))
  gg <- gg + geom_line()
  gg <- gg + labs(x = 'Number of clusters',
                  y = 'Gap statistic')
  gg
}

mshape.plot <- function(fits, links) {
  shape <- as.data.frame(fits$mshape)
  shape <- shape[links, ]
  shape <- cbind(shape, rbind(shape[-1, ], shape[1, ]))
  names(shape) <- c('V1', 'V2', 'V3', 'V4')

  land <- land.frame(fits$rotated)
  gg <- ggplot(land, aes(x = y, y = -x)) + geom_point(alpha = 0.5,
                                                     size = 0.5)
  gg <- gg + geom_segment(data = shape, mapping = aes(x = V2,
                                                      xend = V4,
                                                      y = -V1,
                                                      yend = -V3),
                          alpha = 0.5)
  gg <- gg + geom_point(data = shape, mapping = aes(x = V2,
                                                    y = -V1,
                                                    colour = 'red'),
                        size = 0.9)

  gg
}

shape.plot <- function(shape, links) {
  shape <- as.data.frame(shape)
  shape <- shape[links, ]
  shape <- cbind(shape, rbind(shape[-1, ], shape[1, ]))
  names(shape) <- c('V1', 'V2', 'V3', 'V4')
  gg <- ggplot(shape, aes(x = V2, y = -V1))
  gg <- gg + geom_point(size = 0.9)
  gg <- gg + geom_segment(mapping = aes(x = V2, xend = V4,
                                        y = -V1, yend = -V3))

  gg
}

multishape.plot <- function(shapes, links, labeller = 'label_value') {
  shapes <- lapply(shapes, as.data.frame)
  shapes <- lapply(shapes, function(x) x[links, ])
  shapes <- lapply(shapes, function(x) cbind(x, rbind(x[-1, ], x[1, ])))
  shapes <- lapply(shapes, function(x) {
                   names(x) <- c('V1', 'V2', 'V3', 'V4')
                   x})
  na <- c(rep(seq(length(shapes)), lapply(shapes, nrow)))
  shapes <- Reduce(rbind, shapes)
  shapes <- cbind(shapes, lab = factor(na))
  gg <- ggplot(shapes, aes(x = V2, y = -V1))
  gg <- gg + geom_point(size = 1.3)
  gg <- gg + geom_segment(mapping = aes(x = V2, xend = V4,
                                        y = -V1, yend = -V3))
  gg <- gg + facet_grid(. ~ lab, labeller = labeller)

  gg
}

map.plot <- function(data, label, map, coord = 'gilbert') {
  # wrapper to make a map with labeled points on it
  #
  # Args:
  #   data: points to be plotted
  #         x, y coordinates
  #   label: vector of factors (or coercible to factors) to color points by
  #   map: map information (see ggplot::map_data)
  #   coord: projection type
  #
  # Returns:
  #   object of class ggplot

  data <- cbind(as.data.frame(data), label = label)
  gg <- ggplot(map, aes(x = long, y = lat, group = group))
  gg <- gg + geom_polygon(fill = 'white',
                          colour = 'black')
  gg <- gg + coord_map(coord)
  gg <- gg + geom_point(data = data,
                        mapping = aes(x = long, y = lat,
                                      group = NULL,
                                      colour = factor(label)),
                        size = 1)
  gg
}

class.map <- function(train, test, label, pred, map, facet = TRUE) {
  gg <- ggplot(map, aes(x = long, y = lat, group = group))
  gg <- gg + geom_polygon(fill = 'white', colour = 'black')
  gg <- gg + coord_map('gilbert')

  if(facet) {
    tr <- cbind(long = train$long, lat = train$lat, label = label)
    te <- cbind(long = test$long, lat = test$lat, label = pred)
    dat <- list(train = tr, test = te)
    tt <- c('train', 'test')
    dat <- Map(function(x, y) cbind(as.data.frame(x), type = rep(y, nrow(x))),
               x = dat, y = tt)
    dat <- Reduce(rbind, dat)

    gg <- gg + geom_point(data = dat,
                          mapping = aes(x = long, y = lat, 
                                        group = NULL,
                                        colour = factor(label)),
                          size = 1.25,
                          alpha = 0.7)
    gg <- gg + facet_wrap(~ type)
  } else {
    train <- cbind(train, label = label)
    gg <- gg + geom_point(data = train,
                         mapping = aes(x = long, y = lat,
                                       group = NULL,
                                       colour = factor(label)
                                       ),
                         size = 1.25,
                         alpha = 0.7)
    test <- cbind(test, label = pred)
    gg <- gg + geom_point(data = test,
                          mapping = aes(x = long, y = lat,
                                        group = NULL,
                                        colour = factor(label)
                                        ), 
                          size = 1.5,
                          shape = 15,
                          alpha = 0.8)
  }

  gg
}


clean.resamp <- function(values) {
  # clean up the results of caret so that i can use ggplot to plot them :P
  #
  # Args:
  #  values: x$values where x is result from resamples function to caret object
  #
  # Returns:
  #  object suitable for resamp.plot

  s <- values
  s.m <- melt(s)
  nn <- data.frame(matrix(unlist(strsplit(as.character(s.m[, 2]), 
                                          split = '[~]', perl = TRUE)),
                          ncol = 2, byrow = TRUE))
  s.m <- cbind(s.m[, -2], nn)
  s.m <- split(s.m, f = s.m$X2)
  s.mod <- s.m$Accuracy$X1
  s.acc <- s.m$Accuracy$value
  s.kap <- s.m$Kappa$value
  s.dat <- data.frame(model = s.mod,
                      accuracy = s.acc,
                      kappa = s.kap)
  s.dat <- melt(s.dat)
  s.dat <- ddply(s.dat, .(model, variable), summarize,
                  mean = mean(value),
                  #se = sd(value)# / sqrt(length(value))
                  low = quantile(value, probs = 0.025),
                  high = quantile(value, probs = 0.975)
                  )
  s.dat
}


resamp.plot <- function(dat) {
  # make plot of clean.resamp results
  # 
  # Args:
  #   dat: output from clean.resamp
  #
  # Returns:
  #   object of class ggplot

  g.nre.s <- ggplot(dat, aes(x = model, y = mean))
  g.nre.s <- g.nre.s + geom_pointrange(mapping = aes(ymax = high,
                                                     ymin = low))
  g.nre.s <- g.nre.s + coord_flip()
  g.nre.s <- g.nre.s + facet_wrap(~ variable)
  g.nre.s
}


clean.diff <- function(diffs) {
  # clean up the results of caret so that i can use ggplot to plot them :P
  #
  # Args:
  #  values: x$difs where x is result from diffs of
  #          resamples function to caret object
  #
  # Returns:
  #  object suitable for diffs.plot

  diffs <- melt(diffs)
  out <- ddply(diffs, .(X2, L1), summarize
               , mean = mean(value)
               #, se = sd(value)# / sqrt(length(value))
               #, low = t.test(value, conf.level = conf)$conf.int[1]
               #, high = t.test(value, conf.level = conf)$conf.int[2]
               , low = quantile(value, probs = 0.025)
               , high = quantile(value, probs = 0.975)
               )
  out
}


diffs.plot <- function(dat) {
  # make plot of clean.diff results
  # 
  # Args:
  #   dat: output from clean.diff
  #
  # Returns:
  #   object of class ggplot

  gdd <- ggplot(dat, aes(x = X2, y = mean))
  gdd <- gdd + geom_pointrange(mapping = aes(ymax = high,
                                             ymin = low))
  gdd <- gdd + geom_hline(yintercept = 0) + labs(x = 'Model')
  gdd <- gdd + coord_flip()
  gdd <- gdd + facet_wrap(~ L1)
  gdd
}


grab.letters <- function(string, num) {
  # helper function for creating nice confusion matrix with xtable
  str.vec <- strsplit(string, '')
  str.vec[[1]][seq(num)]
}

let2str <- function(x) {
  # helper function for creating nice confusion matrix with xtable
  paste(x, collapse = '')
}

shorten <- function(x, n = 3) {
  # helper function for creating nice confusion matrix with xtable
  let2str(grab.letters(x, n))
}
 

# model comparison plots that are actually meaningful

