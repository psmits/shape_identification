# bootstrap distributions of AUC ROC values from turtle supervised models
#
# peter d smits
# psmits@uchicago.edu
###############################################################################
library(plyr)
library(cluster)
library(randomForest)
library(caret)
library(pROC)
library(MuMIn)
library(boot)
library(MASS)
library(nnet)

source('../R/multiclass_roc.r')
source('../R/model_comparison.r')

# boot strap the generalization
boot.roc <- function(data, indicies) {
  data <- data[indicies, ]
  pp <- data[, seq(ncol(data) - 1)]
  tt <- data[, ncol(data)]
  return(allvone(pp, tt))
}

# random forests
rr <- list()
for (ii in seq(length(groups))) {
  oo <- cbind(trf.a.analysis$class[[ii]],
              test = adult.test[[ii]][, groups[[ii]]])

  rr[[ii]] <- boot(data = oo, statistic = boot.roc, R = 1000)
}
names(rr) <- groups

# multinomial logistic regression
mm <- list()
for (ii in seq(length(groups))) {
  oo <- cbind(tm.a.analysis$class[[ii]],
              test = adult.test[[ii]][, groups[[ii]]])
  if(length(levels(oo$test)) == 2) {

    bin.boot <- function(data, indicies) {
      data <- data[indicies, ]
      res <- ifelse(data[, 1] == 'marm', 1, 0)
      ob <- ifelse(data[, 3] == 'marm', 1, 0)
      pp <- data[, 2]
      return(roc(ob, pp)$auc)
    }

    mm[[ii]] <- boot(data = oo, statistic = bin.boot, R = 1000)
  } else {
    mm[[ii]] <- boot(data = oo, statistic = boot.roc, R = 1000)
  }
}
names(mm) <- groups

# linear discriminate analysis
ll <- list()
for (ii in seq(length(groups))){
  oo <- cbind(tl.a.analysis$class[[ii]],
              test = adult.test[[ii]][, groups[[ii]]])
  names(oo)[1] <- 'pred'

  ll[[ii]] <- boot(data = oo, statistic = boot.roc, R = 1000)
}
names(ll) <- groups


# lda with rf variables
lrf <- list()
for (ii in seq(length(groups))) {
  oo <- cbind(gr[[ii]], test = adult.test[[ii]][, groups[[ii]]])
  names(oo)[1] <- 'pred'

  lrf[[ii]] <- boot(data = oo, statistic = boot.roc, R = 1000)
}
names(lrf) <- groups

save.image(file = '../data/gen.RData')
