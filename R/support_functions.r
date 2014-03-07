# various utility functions
#
# peter d smits
# psmits@uchicago.edu
###############################################################################
require(caret)

data.maker <- function(gr, data, p = 0.75) {
  nd <- data[, colnames(data) %in% gr]
  out <- apply(nd, 2, createDataPartition,
               p = p, list = FALSE)
  out
} 

make.form <- function(vari, resp) {
  form <- vector(mode = 'list', length = length(vari))
  for (ii in seq(length(vari))) {
    form[[ii]] <- as.formula(paste(paste(resp, ' ~ ', collapse = ''),
                                   paste(vari[seq(ii)],
                                         collapse = '+')))
  }
  form
}

multi.train <- function(form, data, seed = 1, ...) {
  # train across multiple formulas with the same data set
  #
  # Args
  #   form: list of formula objects
  #   data: data frame
  #   seed: random seed
  #   ...: arguments (matched exactly) for train.formula from caret
  #
  # Returns:
  #   list of model training results
  set.seed(seed)
  rr <- lapply(form, train.formula, data = data, ...)
  rr
}


flatten.next <- function(xx) {
  ll <- Filter(function(x) 'list' %in% class(x), xx)
  uu <- Filter(function(x) !('list' %in% class(x)), xx)

  smod <- list()

  for(kk in seq(length(uu))) {
    smod[[length(smod) + 1]] <- uu[[kk]]
  }
  for(ii in seq(length(ll))) {
    for(jj in seq(length(ll[[ii]]))) {
      smod[[length(smod) + 1]] <- ll[[ii]][[jj]]
    }
  }

  uu.nam <- names(uu)
  ll.nam <- names(unlist(Map(function(x, n) rep(x, n),
                             x = names(ll),
                             n = lapply(ll, length))))
  nam <- c(uu.nam, ll.nam)
  names(smod) <- nam

  smod
}
