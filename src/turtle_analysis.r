###############################################################################
##
##  analysis of turtlee plastron data
##  analysis of machine learning results
##
##  peter d smits
##  psmits@uchicago.edu
##
###############################################################################

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

clean.mods <- function(models) {
}

tm <- clean.mods()
tnn <- clean.mods()
trf <- clean.mods()

# multinomial logistic regression
tm.sel <- lapply(tm, function(x)
                 model.sel(lapply(x, function(y) y$finalModel)))

tm.best <- mapply(function(sel, mod) mod[[as.numeric(rownames(sel)[1])]],
                  sel = tm.sel, mod = tm,
                  SIMPLIFY = FALSE)

tm.class <- mapply(predict, tm.best, turtle.test,
                   MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)

tm.conf <- Map(function(x, y) lapply(x, confusionMatrix, y),
               tm.class, classes)


# neural nets
tnn.varimp <- lapply(tnn, function(x) lapply(x[-1], varImp,
                                             scale = TRUE))

tnn.re <- lapply(tnn, resamples)
tnn.redi <- lapply(tnn.re, diff)

tnn.best <- lapply(tnn, function(x, y) x[y],
                   y = 5:6)

tnn.class <- mapply(predict, tnn.best, turtle.test,
                    MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)


# random forests
trf.varimp <- lapply(trf, function(x) lapply(x[-1], varImp,
                                             scale = TRUE))

trf.re <- lapply(trf, resamples)
trf.redi <- lapply(trf.re, diff)

trf.best <- lapply(geo.nnet, function(x, y) x[y],
                   y = 5:6)

trf.class <- mapply(predict, trf.best, turtle.test,
                    MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)

trf.conf <- Map(function(x, y) lapply(x, confusionMatrix, y),
                trf.class, classes)

save.image(file = 'ml_analysis.RData')
