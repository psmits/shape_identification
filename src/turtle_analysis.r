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

tm.a <- clean.mods(tmulti.a)
tnn.a <- clean.mods(tnnet.a)
trf.a <- clean.mods(trf.a)

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


tm.a.sel <- lapply(tm.a, function(x)
                   model.sel(lapply(x, function(y) y$finalModel)))

tm.a.best <- mapply(function(sel, mod) mod[[as.numeric(rownames(sel)[1])]],
                    sel = tm.a.sel, mod = tm.a,
                    SIMPLIFY = FALSE)

tm.a.class <- mapply(predict, tm.a.best, adult.test,
                     MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)

tm.a.conf <- Map(confusionMatrix,
                 tm.a.class, ad.class)


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

tnn.a.varimp <- lapply(tnn.a, function(x) lapply(x[-1], varImp,
                                                 scale = TRUE))

tnn.a.re <- lapply(tnn.a, resamples)
tnn.a.redi <- lapply(tnn.a.re, diff)

tnn.a.best <- Map(function(x, y) x[y], x = tnn.a, y = list(7:8,
                                                           7:8,
                                                           8:9,
                                                           10))

tnn.a.class <- mapply(predict, tnn.a.best, adult.test,
                      MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)

tnn.a.conf <- Map(function(x, y) lapply(x, confusionMatrix, y),
                  tnn.a.class, ad.class)


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

trf.a.varimp <- lapply(trf.a, function(x) lapply(x[-1], varImp,
                                             scale = TRUE))

trf.a.re <- lapply(trf.a, resamples)
trf.a.redi <- lapply(trf.a.re, diff)

trf.a.best <- Map(function(x, y) x[y], x = trf.a, y = list(8:9,
                                                           8:9,
                                                           8:9,
                                                           8:9))

trf.a.class <- mapply(predict, trf.a.best, adult.test,
                    MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)

trf.a.conf <- Map(function(x, y) lapply(x, confusionMatrix, y),
                trf.a.class, ad.class)


# best models compared
#tm.best
#tnn.best
#trf.best

mods <- list()
for(ii in seq(length(tm.best))) {
  mods[[ii]] <- list(multi = tm.best[[ii]],
                     nnet = tnn.best[[ii]],
                     rf = trf.best[[ii]])
}
names(mods) <- c('sh1', 'sh2', 'sh3', 'spinks')

tmod <- lapply(mods, flatten.next)
tmod.re <- lapply(tmod, resamples)
tmod.redi <- lapply(tmod.re, diff)

a.mod <- list()
for(jj in seq(length(tm.a.best))) {
  a.mod[[jj]] <- list(multi = tm.a.best[[jj]],
                      nnet = tnn.a.best[[jj]],
                      rf = trf.a.best[[jj]])
}
names(a.mod) <- c('sh1', 'sh2', 'sh3', 'spinks')

tmod.a <- lapply(mods, flatten.next)
tmod.a.re <- lapply(tmod.a, resamples)
tmod.a.redi <- lapply(tmod.re, diff)


## relative risk and class specific accuracy
t.rr <- lapply(tm.best, function(x) {
               exp(coef(x$finalModel))})
t.a.rr <- lapply(tm.a.best, function(x) {
                 exp(coef(x$finalModel))})



save.image(file = 'turtle_analysis.RData')
