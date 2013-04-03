###############################################################################
##
##  support functions for turtles
##
##  peter d smits
##
###############################################################################

## libraries
require(MASS)
require(utils)
require(shapes)
require(nnet)
require(caret)
require(e1071)

## functions

riem.matrix <- function(config) {
  ##  create a pair-wise Riemannian distance matrix
  ##
  ##  Args:
  ##    config: k x m x n array of (fit) landmarks
  ##
  ##  Returns:
  ##    an n x n symetric matrix of Riemannian shape distances

  n <- dim(config)[3]

  pr <- combn(n, 2)
  dis <- apply(pr, 2, function(x) riemdist(config[, , x[1]], 
                                           config[, , x[2]]))
  mat <- matrix(ncol = n, nrow = n)
  for(ii in seq(ncol(pr))) {
    mat[pr[1, ii], pr[2, ii]] <- dis[ii]
  }

  mat[lower.tri(mat)] <- t(mat[upper.tri(mat)])
  diag(mat) <- 0

  mat  
}

clu <- function(x) {
  ## convenience clustering
  ##
  ##  Args:
  ##    x: square, symmetric distance matrix
  ##
  ##  Returns:
  ##    list with results from agnes and diana

  out <- list()
  out$agglo <- agnes(x)
  out$div <- diana(x)
  out
} 

land.frame <- function(x) {
  ## make a nice data frame from a 3-dimenstional array
  ##
  ##  Args:
  ##    x: 3-way array
  ##
  ##  Returns:
  ##    data frame that is useful with ggplot

  m <- dim(x)[2]
  n <- dim(x)[3]
  dat <- x[, , 1]
  for (ii in seq(from = 2, to = n)) {
    dat <- rbind(dat, x[, , ii])
  }

  dat <- as.data.frame(dat)
  if (m == 2) nam <- c('x', 'y') else if (m == 3) nam <- c('x', 'y', 'z')
  names(dat) <- nam

  dat
}

multi.mod <- function(data, formula, preProc) {
  ## fit multinomial logistic regression to data
  ## this is so i can map over a list
  ##
  ##  Args:
  ##    data: object of class data.frame, probably the training set
  ##    formula: the formula for the multinomial logistic regression
  ##    trControl: input for trControl in train
  ##    preProc: input for preProc in train
  ##
  ##  Returns:
  ##    object of class train with all the correct attributes

  mod <- train(formula, data = data, 
               method = 'multinom', 
               preProc = preProc)

  mod
}

data.maker <- function(gr, data, p = 0.75) {
  nd <- data[, colnames(data) %in% gr]
  out <- apply(nd, 2, createDataPartition,
               p = p, list = FALSE)
  out
} 

require(caret)
ctrl <- trainControl(#method = 'LOOCV',
                     method = 'repeatedCV',
                     #classProbs = TRUE,
                     number = 10,
                     repeats = 10)

make.form <- function(vari, resp) {
  form <- vector(mode = 'list', length = length(vari))
  for (ii in seq(length(vari))) {
    form[[ii]] <- as.formula(paste(paste(resp, ' ~ ', collapse = ''),
                                   paste(vari[seq(ii)],
                                         collapse = '+')))
  }
  form
}

data.maker <- function(gr, data, p = 0.75) {
  nd <- data[, colnames(data) %in% gr]
  out <- apply(nd, 2, createDataPartition,
               p = p, list = FALSE)
  out
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
