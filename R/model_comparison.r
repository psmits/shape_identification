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
tm.a.analysis <- multi.analysis(tm.a, ad.class, adult.test)

# random forests
trf.a <- clean.mods(trf.a)
trf.a.analysis <- rf.analysis(trf.a, ad.class, adult.test)

# linear discriminate analysis
tl.a <- clean.mods(tlda.a)
tl.a.analysis <- lda.analysis(tl.a, adult.test)

# best models
#a.mod <- list()
#for(jj in seq(length(groups))) {
#  a.mod[[jj]] <- list(multi = tm.a.analysis$best[[jj]],
#                      rf = trf.a.analysis$best[[jj]],
#                      lda = tl.a.analysis$best[[jj]])
#}
#names(a.mod) <- c('sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks')
#tmod.a <- a.mod

# relative risk and class specific accuracy
t.a.rr <- lapply(tm.a.analysis$best, function(x) {
                 exp(coef(x$finalModel))})
t.a.rr.ci <- lapply(tm.a.analysis$best, function(x) {
                    exp(confint(x$finalModel))})
