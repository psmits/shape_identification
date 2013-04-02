###############################################################################
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
source('../src/turtle_mung.r')

load('../src/cluster_res.RData')
load('../src/supervised_misc.RData')
load('../src/multi_boot_mod.RData')
load('../src/nnet_boot_mod.RData')
load('../src/rf_boot_mod.RData')

clean.mods <- function(models, lab = c('sh1', 'sh2', 'sh3', 'spinks')) {
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

tm <- clean.mods(tmulti)
tnn <- clean.mods(tnnet)
trf <- clean.mods(trf)

# multinomial logistic regression
tm.sel <- lapply(tm, function(x)
                 model.sel(lapply(x, function(y) y$finalModel)))

tm.best <- mapply(function(sel, mod) mod[[as.numeric(rownames(sel)[1])]],
                  sel = tm.sel, mod = tm,
                  SIMPLIFY = FALSE)

tm.class <- mapply(predict, tm.best, turtle.test,
                   MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)

tm.conf <- Map(confusionMatrix,
               tm.class, classes)


# neural nets
tnn.varimp <- lapply(tnn, function(x) lapply(x[-1], varImp,
                                             scale = TRUE))

tnn.re <- lapply(tnn, resamples)
tnn.redi <- lapply(tnn.re, diff)

tnn.best <- Map(function(x, y) x[y], x = tnn, y = list(8:9,
                                                       7:8,
                                                       9:10,
                                                       8:9))

tnn.class <- mapply(predict, tnn.best, turtle.test,
                    MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)

tnn.conf <- Map(function(x, y) lapply(x, confusionMatrix, y),
                tnn.class, classes)

# random forests
trf.varimp <- lapply(trf, function(x) lapply(x[-1], varImp,
                                             scale = TRUE))

trf.re <- lapply(trf, resamples)
trf.redi <- lapply(trf.re, diff)

trf.best <- Map(function(x, y) x[y], x = trf, y = list(10,
                                                       10,
                                                       9:10,
                                                       8:9))

trf.class <- mapply(predict, trf.best, turtle.test,
                    MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)

trf.conf <- Map(function(x, y) lapply(x, confusionMatrix, y),
                trf.class, classes)

save.image(file = 'turtle_analysis.RData')
