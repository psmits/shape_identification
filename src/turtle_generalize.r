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
require(boot)
require(MASS)

source('../src/support_functions.r')

load('../data/turtle_analysis.RData')

# boot strap the generalization

boot.roc <- function(data, indicies) {
  data <- data[indicies, ]
  pp <- data[, seq(ncol(data) - 1)]
  tt <- data[, ncol(data)]
  return(allvone(pp, tt))
}

rr <- list()
for (ii in seq(length(groups))) {
  oo <- cbind(trf.a.analysis$class[[ii]],
              test = adult.test[[ii]][, groups[[ii]]])

  rr[[ii]] <- boot(data = oo, statistic = boot.roc, R = 1000)
}
names(rr) <- groups

mm <- list()
for (ii in seq(length(groups))) {
  oo <- cbind(tm.a.analysis$class[[ii]],
              test = adult.test[[ii]][, groups[[ii]]])

  mm[[ii]] <- boot(data = oo, statistic = boot.roc, R = 1000)
}
names(mm) <- groups

ll <- list()
for (ii in seq(length(groups))){
  oo <- cbind(tl.a.analysis$class[[ii]],
              test = adult.test[[ii]][, groups[[ii]]])

  ll[[ii]] <- boot(data = oo, statistic = boot.roc, R = 1000)
}

save(rr, mm, ll, file = '../data/turtle_gen.RData')
