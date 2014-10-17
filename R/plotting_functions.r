###############################################################################
##
##  plotting and tables for the turtle data
##
##  peter d smits
##  psmits@uchicago.edu
##
###############################################################################

source('../R/support_functions.r')

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

mshape.plot <- function(fits, links, snd.links) {
  shape1 <- as.data.frame(fits$mshape)
  shape <- shape1[links, ]
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

  snd.shape <- shape1[snd.links, ]
  ss <- seq(from = 1, to = nrow(snd.shape), by = 2)
  snd.shape <- cbind(snd.shape[ss, ], snd.shape[ss + 1, ])
  names(snd.shape) <- names(shape)
  for(ii in seq(from = 1, to = length(snd.links), by = 2)) {
    gg <- gg + geom_segment(data = snd.shape[c(ii, ii + 1), ], 
                            mapping = aes(x = V2,
                                          xend = V4,
                                          y = -V1,
                                          yend = -V3))
  }

  gg <- gg + geom_point(data = shape, mapping = aes(x = V2,
                                                    y = -V1,
                                                    colour = 'red'),
                        size = 0.9)

  gg
}

map.plot <- function(data, label, map) {
  # wrapper to make a map with labeled points on it
  #
  # Args:
  #   data: points to be plotted
  #         x, y coordinates
  #   label: vector of factors (or coercible to factors) to color points by
  #   map: output from get_map
  #
  # Returns:
  #   object of class ggplot

  data <- cbind(as.data.frame(data), lab = label)
  gg <- ggmap(map)
  gg <- gg + geom_point(data = data,
                        mapping = aes(x = long, y = lat,
                                      group = NULL,
                                      shape = factor(lab)))
  gg
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


#' function to make map of prediction results
#'
#' @param map ggmap object
#' @param xlim vector length 2; longitude min, longitude max
#' @param ylim vector length 2; latitude min, latitude max
#' @param test df; testing data set
#' @param name character; classification scheme
#' @param mods list; list of model out data
#' @param types vector; names of different models
#' @param data df; original data
pred.map <- function(map, xl, yl, test, name, mods, types, data, cbp = cbp) {
  gg <- map + xlim(xl[1] - 1, xl[2]) + ylim(yl[1], yl[2])
  tr <- cbind(long = test[[name]]$long,
              lat = test[[name]]$lat,
              label = as.character(test[[name]][, name]))
  tr <- cbind(data.frame(tr), type = rep('testing', nrow(tr)))
  te <- cbind(long = test[[name]]$long,
              lat = test[[name]]$lat)
  mty <- do.call(c, llply(types, function(x) rep(x, nrow(te))))
  te <- do.call('rbind', replicate(length(types), te, simplify = FALSE))

  # extract the predicted classes
  
  label <- unlist(lapply(mods, function(x) as.character(x$class[[name]][, 1])))
  te <- cbind(te,
              label = as.character(label),
              type = mty)
  turts <- rbind(tr, te)
  turts[, 1:2] <- apply(turts[, 1:2], 2, function(x) as.numeric(as.character(x)))

  gg <- gg + geom_point(data = turts,
                        mapping = aes(x = long, y = lat,
                                      group = NULL,
                                      colour = factor(label)))
  data$happy <- data[, name]
  gg <- gg + stat_ellipse(geom = 'polygon',
                          data = data,
                          mapping = aes(x = long,
                                        y = lat,
                                        fill = happy),
                          alpha = 0.35)
  gg <- gg + facet_wrap(~ type)
  gg
}


#' plot of most important PC axes
#' 
#' @param imp vector; most important PC axes
#' @param many numeric; how many axes?
#' @param what character; which classification scheme?
#' @param data df; data to plot
imp.pairs <- function(imp, many, what, data, cbp = cbp) {
  # surface to place on
  ggimp <- ggpairs(data,
                   columns = c(imp[seq(many)], what),
                   colour = what,
                   upper = 'blank',
                   lower = 'blank',
                   params = c(LabelSize = 2, gridLabelSize = 2, size = 1))

  data$happy <- data[, what]
  # individual plots of each axis
  pair <- combn(seq(many), 2, simplify = FALSE)
  pcs <-  lapply(pair, function(x) {
                 nn <- laply(x, function(x) paste('PC', x, sep = ''))
                 g <- ggplot(data, mapping = aes_string(x = nn[2], 
                                                        y = nn[1], 
                                                        colour = what))
                 g <- g + geom_point() + scale_color_manual(values = cbp)})

  # histograms
  hists <- lapply(imp[seq(many)], function(x) {
                  g <- ggplot(data, mapping = aes_string(x = x, fill = what))
                  g <- g + geom_histogram()
                  g <- g + facet_grid(happy ~ .)
                  g <- g + scale_fill_manual(values = cbp)
                  g})

  for(ii in seq(length(pcs))) {
    ggimp <- putPlot(ggimp, pcs[[ii]], pair[[ii]][2], pair[[ii]][1])
  }
  for(ii in seq(length(hists))) {
    ggimp <- putPlot(ggimp, hists[[ii]], many + 1, ii)
  }

  ggimp
}


#' class mean plots
#'
#' @param scheme character; classification scheme
#' @param shapes 3-way array; landmark locations
class.mean <- function(scheme, shapes, data) {
  spi <- data[, scheme]
  wspi <- lapply(levels(spi), function(x, y) which(y == x), y = spi)
  mspi <- lapply(wspi, function(x, y) mshape(y[, , x]), y = shapes)
  mins <- lapply(mspi, function(x) min(x[, 2]))
  mins <- min(unlist(mins))
  lmat <- cbind(links, c(links[-1], links[1]))
  lmat <- rbind(lmat, matrix(snd.links, nrow = 5, byrow = TRUE))
  mspi <- lapply(mspi, function(x) x[, 2:1])
  mspi <- lapply(mspi, function(x) {
                 x[, 2] <- -1 * x[, 2]
                 x})
  mspi
}


#' shape variation along first two most important axes
#'
#' @param imp vector; names of most important axes
#' @param data df; data to plot
#' @param shape 3-way array; shape data
#' @param links order of links between points
#' @param snd.links 
shape.imp <- function(imp, data, shape, links, snd.links) {
  fst.max <- which.max(data[, imp[1]])
  fst.min <- which.min(data[, imp[1]])
  snd.max <- which.max(data[, imp[2]])
  snd.min <- which.min(data[, imp[2]])

  ex.lab <- function(var, value) {
    value <- as.character(value)
    if(var == 'lab') {
      value[value == '1'] <- 'min'
      value[value == '2'] <- 'mean'
      value[value == '3'] <- 'max'
    }
    return(value)
  }

  mt <- mshape(shape)

  comps <- list(shape[, , fst.min], shape[, , fst.max],
                mt,
                shape[, , snd.min], shape[, , snd.max], 
                mt)
  snd.com <- comps <- lapply(comps, as.data.frame)
  comps <- lapply(comps, function(x) x[links, ])
  comps <- lapply(comps, function(x) cbind(x, rbind(x[-1, ], x[1, ])))
  comps <- lapply(comps, function(x) {
                  names(x) <- c('V1', 'V2', 'V3', 'V4')
                  x})
  comps <- Reduce(rbind, comps)
  shl <- unlist(lapply(imp[1:2], function(x) rep(x, 3 * nrow(mt))))
  shl <- factor(shl, levels = imp[1:2])
  typ <- c(rep('min', nrow(mt)), rep('max', nrow(mt)))
  mm <- rep('mean', nrow(mt))
  typ <- c(typ, mm, typ, mm)
  varshape <- cbind(comps, shl, typ)

  # second links
  ss <- seq(from = 1, to = length(snd.links), by = 2)
  scl <- lapply(snd.com, function(x) x[snd.links, ])
  scl <- lapply(scl, function(x) cbind(x[ss, ], x[ss + 1, ]))
  scl <- lapply(scl, function(x) {
                names(x) <- c('V1', 'V2', 'V3', 'V4')
                x})
  scl <- Reduce(rbind, scl)
  shl <- unlist(lapply(imp[1:2], 
                       function(x) rep(x, 3 * length(snd.links) / 2)))
  shl <- factor(shl, levels = imp[1:2])
  names(shl) <- NULL
  typ <- c(rep('min', length(snd.links) / 2), 
           rep('max', length(snd.links) / 2))
  mm <- rep('mean', length(snd.links) / 2)
  typ <- c(typ, mm, typ, mm)
  scl <- cbind(scl, shl, typ)

  gsh <- ggplot(varshape, aes(x = V2, y = -V1)) + geom_point()
  gsh <- gsh + geom_segment(mapping = aes(x = V2, xend = V4,
                                          y = -V1, yend = -V3))
  for(ii in seq(from = 1, to = nrow(scl), by = 2)) {
    gsh <- gsh + geom_segment(data = scl[c(ii, ii + 1), ],
                              mapping = aes(x = V2,
                                            xend = V4,
                                            y = -V1,
                                            yend = -V3))
  }
  gsh <- gsh + facet_grid(shl ~ typ)
  gsh <- gsh + theme(axis.title = element_blank(),
                     axis.text = element_blank())
  gsh
}


#' relative risk plots
#'
#' @param relrisk matrix
#' @param relci matrix
#' @param imp vector
#' @param many numeric
relrisk.plot <- function(relrisk, relci, imp, many) {
  rel <- list()
  for(ii in seq(nrow(relrisk))) {
    rel[[ii]] <- as.data.frame(cbind(relrisk[ii, imp[seq(many)]], relci[imp[seq(many)], , ii]))
    colnames(rel[[ii]]) <- c('rr', 'bot', 'up')
  }
  names(rel) <- c('eastern', 'western', 'southern')
  rel <- lapply(rel, function(x) cbind(x, pc = rownames(x)))
  rel <- mapply(function(x, y) cbind(x, class = rep(y, nrow(x))), 
                x = rel, y = names(rel),
                SIMPLIFY = FALSE)
  rel <- Reduce(rbind, rel)


  rel$pc <- factor(rel$pc, levels = imp)
  rel <- rel[rel$pc %in% imp[seq(many)], ]
  ggrel <- ggplot(rel, aes(x = class, y = rr, ymax = up, ymin = bot)) 
  ggrel <- ggrel + geom_pointrange()
  ggrel <- ggrel + geom_hline(aes(yintercept = 0), lty = 2)
  ggrel <- ggrel + facet_wrap(~ pc)
  ggrel <- ggrel + labs(y = 'relative risk')
  ggrel <- ggrel + theme(axis.text = element_text(size = 9)) 

  ggrel
}
