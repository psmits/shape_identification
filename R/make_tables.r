# various tables necessary for the paper
library(xtable)
library(reshape2)
library(plyr)
library(boot)
library(pROC)
library(Metrics)

source('../R/plotting_functions.r')
source('../R/miss_class.r')

load('../data/gen.RData')

rr$sh1$t0
sd(rr$sh1$t)

rr.comp <- data.frame(sch = names(rr), 
                      train.mean = unlist(Map(function(x, y) x[y], 
                                              trf.a.analysis$auc, 
                                              trf.a.analysis$best)),
                      test.mean = laply(rr, function(x) x$t0), 
                      test.sd = laply(rr, function(x) sd(x$t, na.rm = TRUE)))
mm.comp <- data.frame(sch = names(mm), 
                      train.mean = laply(tm.a.analysis$auc, max),
                      test.mean = laply(mm, function(x) as.numeric(x$t0)), 
                      test.sd = laply(mm, function(x) sd(x$t, na.rm = TRUE)))
ll.comp <- data.frame(sch = names(ll), 
                      train.mean = laply(tl.a.analysis$auc, max),
                      test.mean = laply(ll, function(x) x$t0), 
                      test.sd = laply(ll, function(x) sd(x$t, na.rm = TRUE)))
lrf.comp <- data.frame(sch = names(lrf), 
                       train.mean = laply(trf.a.analysis$auc, max),
                       gen.mean = laply(lrf, function(x) x$t0), 
                       gen.sd = laply(lrf, function(x) sd(x$t, na.rm = TRUE)))

which.max(rr.comp[, 2]) == which.max(rr.comp[, 3])
which.max(mm.comp[, 2]) == which.max(mm.comp[, 3])
which.max(ll.comp[, 2]) == which.max(ll.comp[, 3])
which.max(lrf.comp[, 2]) == which.max(lrf.comp[, 3])

comp.tab <- cbind(rr.comp, mm.comp[, -1], ll.comp[, -1], lrf.comp[, -1])
comp.tab <- xtable(comp.tab)
label(comp.tab) <- 'comp_tab'
print.xtable(x = comp.tab, 
             file = '../doc/comp_tab_raw.tex')
