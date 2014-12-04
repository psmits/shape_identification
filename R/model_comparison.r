# analysis of machine learning results
#
# peter d smits
# psmits@uchicago.edu
###############################################################################

library(MASS)
library(nnet)
library(plyr)
library(cluster)
library(randomForest)
library(caret)
library(pROC)
library(MuMIn)

source('../R/support_functions.r')
source('../R/multi_funcs.r')
source('../R/multiclass_roc.r')
source('../R/supervised_analysis.r')

# covenience function to make the models play nice
clean.mods <- function(models, lab = c('sh1', 'sh2', 'sh3', 
                                       'sh4', 'sh5', 'spinks')) {
  if (!is.null(dim(models))) {
    mm <- alply(models, 2)
    names(mm) <- lab
  } else {
    breaks <- length(models)
    mm <- list()
    for (ii in seq(breaks)) {
      mm[[ii]] <- models[[ii]]
    }
    names(mm) <- lab
  }
  mm
}

# multinomial logicistic regression
tm.a <- clean.mods(tmulti.a)
tm.a.analysis <- multi.analysis(tm.a, class = groups, 
                                test = adult.test, train = adult.train)

# random forests
trf.a <- clean.mods(trf.a)
trf.a.analysis <- rf.analysis(trf.a, ad.class, adult.test)

# linear discriminate analysis
tl.a <- clean.mods(tlda.a)
tl.a <- lapply(tl.a, function(x) x[-1])
tl.a.analysis <- lda.analysis(tl.a, adult.train, adult.test, groups)


# use the random forest importance for the LDA
rf.best <- lapply(trf.a, function(x) x$optVariables)
rf.lda <- Map(function(x, y, z) lda(x[, z], x[, y]), 
              x = adult.train, y = groups, z = rf.best)

gr <- list()
for(ii in seq(length(rf.lda))) {
  guess <- predict(rf.lda[[ii]], adult.test[[ii]][, rf.best[[ii]]])
  gr[[ii]] <- data.frame(cbind(guess$class, guess$posterior))
}
names(gr) <- groups

fix.names <- function(cc) {
  ff <- apply(cc, 1, function(x) {
              nn <- names(x[-1])
              nn[x[1]]})
  if(all(grepl('X', ff))) {
    ff <- gsub('X', '', ff)
  }
  cc[, 1] <- ff
  names(cc)[-1] <- gsub('X', '', names(cc)[-1])
  cc
}
gr <- lapply(gr, fix.names)
