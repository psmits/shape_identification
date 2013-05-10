##############################################################################
##
##  analysis of turtlee plastron data
##  analysis of machine learning results
##
##  peter d smits
##  psmits@uchicago.edu
##
###############################################################################

require(plyr)
require(cluster)
require(nnet)
require(randomForest)
require(caret)
require(e1071)
require(pROC)
require(MuMIn)

source('../src/support_functions.r')

load('../data/turtle_analysis.RData')

# randomly reassign the classes of the testing data set
# predict the permuted testing data set
# this creates a distribution of estimated subspecies
#
# can then see if ROC is significant


pred.dist <- function(best, train, nsim = 1000) {

  resample.class <- function(data, class) {
    data[, class] <- sample(data[, class])
    data
  }

  re.dist <- function(best, train, group, nsim = 1000) {
    pred <- vector(mode = 'list', length = nsim)
    for(ii in seq(nsim)) {
      re <- resample.class(data = train, class = group)
      re.pred <- predict(best, re, type = 'raw')
      pred[[ii]] <- re.pred
    }
    pred
  }

  groups <- names(train)
  out <- mapply(re.dist, best = best, train = train, group = groups,
                MoreArgs = list(nsim = nsim), SIMPLFY = FALSE)

  out
}


# multinomial
multi.dist <- pred.dist(tm.analysis$best, turtle.train)

# random forest
