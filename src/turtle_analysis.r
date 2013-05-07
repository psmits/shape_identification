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
#source('../src/turtle_mung.r')

load('../src/cluster_res.RData')
load('../src/supervised_misc.RData')
load('../src/multi_boot_mod.RData')
#load('../src/nnet_boot_mod.RData')
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
tm.s <- clean.mods(tmulti.s)
#tnn <- clean.mods(tnnet)
trf <- clean.mods(trf)
trf.s <- clean.mods(trf.s)

tm.a <- clean.mods(tmulti.a)
tm.a.s <- clean.mods(tmulti.a.s)
#tnn.a <- clean.mods(tnnet.a)
trf.a <- clean.mods(trf.a)
trf.a.s <- clean.mods(trf.a.s)

multi.analysis <- function(model, class, test) {
  out <- list()
  out$sel <- lapply(model, function(x)
                    model.sel(lapply(x, function(y) y$finalModel)))
  out$best <- mapply(function(sel, mod) mod[[as.numeric(rownames(sel)[1])]],
                     sel = out$sel, mod = model,
                     SIMPLIFY = FALSE)
  out$class <- mapply(predict, out$best, test,
                      MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)
  out$conf <- Map(confusionMatrix,
                  out$class, class)

  out
}

rf.analysis <- function(model, class, test) {
  out <- list()
  out$varimp <- lapply(model, varImp)
  out$re <- resamples(model)
  out$redi <- resamples(out$re)
  out$class <- mapply(predict, model, test,
                      SIMPLIFY = FALSE)
  out$conf <- Map(function(x, y) confusionMatrix(x$pred, y),
                  x = out$class, y = class)
  out
}

# multinomial logistic regression
tm.analysis <- multi.analysis(tm, classes, turtle.test)
tm.s.analysis <- multi.analysis(tm.s, classes, turtle.test)

tm.a.analysis <- multi.analysis(tm.a, ad.class, adult.test)
tm.a.s.analysis <- multi.analysis(tm.a.s, ad.class, adult.test)


# random forests
trf.analysis <- rf.analysis(trf, classes, turtle.test)
trf.s.analysis <- rf.analysis(trf.s, classes, turtle.test)

trf.a.analysis <- rf.analysis(trf.a, ad.class, adult.test)
trf.a.s.analysis <- rf.analysis(trf.a.s, ad.class, adult.test)


# best models compared
#tm.best
#trf.best

mods <- list()
for(ii in seq(length(tm.best))) {
  mods[[ii]] <- list(multi = tm.analysis[[ii]]$best,
#                     nnet = tnn[[ii]],
                     rf = trf.analysis[[ii]]$best)
}
names(mods) <- c('sh1', 'sh2', 'sh3', 'spinks')

#tmod <- lapply(mods, flatten.next)
tmod <- mods
tmod.re <- lapply(tmod, resamples)
tmod.redi <- lapply(tmod.re, diff)

a.mod <- list()
for(jj in seq(length(tm.a.best))) {
  a.mod[[jj]] <- list(multi = tm.a.best[[jj]],
#                      nnet = tnn.a[[jj]],
                      rf = trf.a[[jj]])
}
names(a.mod) <- c('sh1', 'sh2', 'sh3', 'spinks')

#tmod.a <- lapply(a.mod, flatten.next)
tmod.a <- a.mod
tmod.a.re <- lapply(tmod.a, resamples)
tmod.a.redi <- lapply(tmod.re, diff)


## relative risk and class specific accuracy
t.rr <- lapply(tm.analysis$best, function(x) {
               exp(coef(x$finalModel))})
t.rr.ci <- lapply(tm.analysis$best, function(x) {
                  exp(confint(x$finalModel))})
t.a.rr <- lapply(tm.a.analysis$best, function(x) {
                 exp(coef(x$finalModel))})
t.a.rr.ci <- lapply(tm.a.analysis$best, function(x) {
                    exp(confint(x$finalModel))})


save.image(file = 'turtle_analysis.RData')




# neural nets
#tnn.varimp <- lapply(tnn, varImp)

#tnn.re <- resamples(tnn)
#tnn.redi <- diff(tnn.re)

#tnn.class <- mapply(predict, tnn, turtle.test,
#                    SIMPLFIY = FALSE)

#tnn.conf <- Map(function(x, y) confusionMatrix(x$pred, y),
#                x = tnn.class, y = classes)

#tnn.a.varimp <- lapply(tnn.a, varImp)

#tnn.a.re <- resamples(tnn.a)
#tnn.a.redi <- diff(tnn.a.re)

#tnn.a.class <- mapply(predict, tnn.a, adult.test,
#                      SIMPLIFY = FALSE)

#tnn.a.conf <- Map(function(x, y) confusionMatrix(x$pred, y),
#                  tnn.a.class, ad.class)
