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

#load('../data/cluster_res.RData')
load('../data/supervised_misc.RData')
load('../data/multi_boot_mod.RData')
#load('../src/nnet_boot_mod.RData')
load('../data/rf_boot_mod.RData')

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

#tm <- clean.mods(tmulti)
#tm.s <- clean.mods(tmulti.s)
#tnn <- clean.mods(tnnet)
#trf <- clean.mods(trf)
#trf.s <- clean.mods(trf.s)

tm.a <- clean.mods(tmulti.a)
#tm.a.s <- clean.mods(tmulti.a.s)
#tnn.a <- clean.mods(tnnet.a)
trf.a <- clean.mods(trf.a)
#trf.a.s <- clean.mods(trf.a.s)

multi.analysis <- function(model, class, test) {
  out <- list()
  out$sel <- lapply(model, function(mod) model.sel(lapply(mod, function(x) x$finalModel)))
  out$best <- mapply(function(sel, mod) mod[[as.numeric(rownames(sel)[1])]],
                     sel = out$sel, mod = model,
                     SIMPLIFY = FALSE)
  pc <- mapply(predict, out$best, test,
               MoreArgs = list(type = 'raw'), SIMPLIFY = FALSE)
  pp <- vector(mode = 'list', length = length(out$best))
  for(ii in seq(length(out$best))) {
    pp[[ii]] <- predict(out$best[[ii]]$finalModel, test[[ii]], type = 'probs')
  }
  out$class <- mapply(function(x, y) cbind(pred = x, as.data.frame(y)), 
                      x = pc, y = pp, SIMPLIFY = FALSE)
    
  #out$conf <- Map(confusionMatrix,
  #                out$class$class, class)

  out
}

rf.analysis <- function(model, class, test) {
  out <- list()
  out$varimp <- lapply(model, varImp)
  out$re <- resamples(model)
  out$class <- mapply(predict, model, test,
                      SIMPLIFY = FALSE)
  out$conf <- Map(function(x, y) confusionMatrix(x$pred, y),
                  x = out$class, y = class)
  out
}

# multinomial logistic regression
#tm.analysis <- multi.analysis(tm, classes, turtle.test)
#tm.s.analysis <- multi.analysis(tm.s, classes, turtle.test)

tm.a.analysis <- multi.analysis(tm.a, ad.class, adult.test)
#tm.a.s.analysis <- multi.analysis(tm.a.s, ad.class, adult.test)


# random forests
#trf.analysis <- rf.analysis(trf, classes, turtle.test)
#trf.s.analysis <- rf.analysis(trf.s, classes, turtle.test)

trf.a.analysis <- rf.analysis(trf.a, ad.class, adult.test)
#trf.a.s.analysis <- rf.analysis(trf.a.s, ad.class, adult.test)


# best models compared
#tm.best
#trf.best

#mods <- list()
#for(ii in seq(length(groups))) {
#  mods[[ii]] <- list(multi = tm.analysis$best[[ii]],
##                     nnet = tnn[[ii]],
#                     rf = trf.analysis[[ii]]$best)
#}
#names(mods) <- c('sh1', 'sh2', 'sh3', 'spinks')

#tmod <- lapply(mods, flatten.next)
#tmod <- mods
#tmod.re <- lapply(tmod, resamples)
#tmod.redi <- lapply(tmod.re, diff)

a.mod <- list()
for(jj in seq(length(groups))) {
  a.mod[[jj]] <- list(multi = tm.a.analysis$best[[jj]],
#                      nnet = tnn.a[[jj]],
                      rf = trf.a.analysis[[jj]]$best)
}
names(a.mod) <- c('sh1', 'sh2', 'sh3', 'spinks')

#tmod.a <- lapply(a.mod, flatten.next)
tmod.a <- a.mod
#tmod.a.re <- lapply(tmod.a, resamples)
#tmod.a.redi <- lapply(tmod.re, diff)


## relative risk and class specific accuracy
#t.rr <- lapply(tm.analysis$best, function(x) {
#               exp(coef(x$finalModel))})
#t.rr.ci <- lapply(tm.analysis$best, function(x) {
#                  exp(confint(x$finalModel))})
t.a.rr <- lapply(tm.a.analysis$best, function(x) {
                 exp(coef(x$finalModel))})
t.a.rr.ci <- lapply(tm.a.analysis$best, function(x) {
                    exp(confint(x$finalModel))})


save.image(file = '../data/turtle_analysis.RData')
