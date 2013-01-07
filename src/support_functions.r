###############################################################################
##
##  support functions
##
##  peter d smits
##
###############################################################################

## libraries
require(utils)
require(shapes)
require(nnet)

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

predictMNL <- function(model, newdata) {
  ## function to predict multinomial logit choice model outcomes
  ## this is from http://www.jameskeirstead.ca/r/how-to-multinomial-regression-models-in-r/
  ##
  ##  Args:
  ##    model: nnet class multinomial model
  ##    newdata: data frame containing new values to predict
  ##
  ##  Returns:
  ##    vector of ids

  if (is.element('nnet', class(model))) {
    # calculate the individual and cumulative probabilities
    probs <- predict(model, newdata, 'probs')
    cum.probs <- t(apply(probs, 1, cumsum))

    # draw random values
    vals <- runif(nrow(newdata))

    # join cumulative probabilities and random draws
    tmp <- cbind(cum.probs, vals)

    # for each row, get choice index
    k <- ncol(probs)
    ids <- 1 + apply(tmp, 1, function(x) length(which(x[1:k] < x[k + 1])))

    # return vector of ids
    ids
  }
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
