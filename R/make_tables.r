# various tables necessary for the paper
library(xtable)
library(reshape2)
library(plyr)

source('../R/plotting_functions.r')

load('../data/gen.RData') 
#load('../data/shape.RData')


against.best <- function(wb, mods) {
  best <- mods[[wb]]$t
  nm <- seq(length(mods))
  nm <- nm != wb
  others <- mods[nm]
  sets <- lapply(others, function(x) x$t)
  nana <- lapply(sets, function(x) which(!is.na(x)))
  dif <- Map(function(x, n) sum((best[n] - x[n]) > 0) / length(x[n]),
             x = sets, n = nana)
  dif
}

#generalize comparisons
best.mm <- which.max(laply(mm, function(x) mean(x$t)))
best.rr <- which.max(laply(rr, function(x) mean(x$t)))
best.ll <- which.max(laply(ll, function(x) mean(x$t)))

mm.dif <- against.best(best.mm, mm)
rr.dif <- against.best(best.rr, rr)
ll.dif <- against.best(best.ll, ll)


# multinomial logistic regression
mm.comp <- cbind(sh1 = mm$sh1$t, sh2 = mm$sh2$t, sh3 = mm$sh3$t, 
                 sh4 = mm$sh4$t, sh5 = mm$sh5$t, spinks = mm$spinks$t)
mm.c <- melt(mm.comp)
mm.res <- with(mm.c, {
               pairwise.t.test(value, Var2)})
mm.tab <- mm.res$p.value
mm.tab <- rbind('1' = rep(NA, 5), mm.tab)
mm.tab <- cbind(mm.tab, '6' = rep(NA, 6))
#colnames(mm.tab) <- coln
#rownames(mm.tab) <- rown
mm.tabx <- xtable(as.table(mm.tab))
align(mm.tabx) <- 'r|rrrrrr'
print.xtable(mm.tabx, 
             file = '../doc/mm_tabx.tex',
             hline.after = c(0, nrow(mm.tabx)),
             include.rownames = TRUE,
             floating = FALSE)

# random forests
rf.comp <- cbind(sh1 = rr$sh1$t, sh2 = rr$sh2$t, sh3 = rr$sh3$t, 
                 sh4 = rr$sh4$t, sh5 = rr$sh5$t, spinks = rr$spinks$t)
rf.c <- melt(rf.comp)
rf.res <- with(rf.c, {
               pairwise.t.test(value, Var2)})
rf.tab <- rf.res$p.value
rf.tab <- rbind('1' = rep(NA, 5), rf.tab)
rf.tab <- cbind(rf.tab, '6' = rep(NA, 6))
#colnames(rf.tab) <- coln
#rownames(rf.tab) <- rown
rf.tabx <- xtable(as.table(rf.tab))
align(rf.tabx) <- 'r|rrrrrr'
print.xtable(rf.tabx, 
             file = '../doc/rf_tabx.tex',
             hline.after = c(0, nrow(mm.tabx)),
             include.rownames = TRUE,
             floating = FALSE)

# linear discriminate analysis
lda.comp <- cbind(sh1 = ll[[1]]$t, sh2 = ll[[2]]$t, sh3 = ll[[3]]$t,
                  sh4 = ll[[2]]$t, sh5 = ll[[5]]$t, spinks = ll[[5]]$t)
lda.c <- melt(lda.comp)
lda.res <- with(lda.c, {
                pairwise.t.test(value, Var2)})
lda.tab <- lda.res$p.value
lda.tab <- rbind('1' = rep(NA, 5), lda.tab)
lda.tab <- cbind(lda.tab, '6' = rep(NA, 6))
#colnames(lda.tab) <- coln
#rownames(lda.tab) <- rown
lda.tabx <- xtable(as.table(lda.tab))
align(lda.tabx) <- 'r|rrrrrr'
print.xtable(lda.tabx, 
             file = '../doc/lda_tabx.tex',
             hline.after = c(0, nrow(mm.tabx)),
             include.rownames = TRUE,
             floating = FALSE)

# lda + rf
lrf.comp <- cbind(sh1 = lrf[[1]]$t, sh2 = lrf[[2]]$t, sh3 = lrf[[3]]$t,
                  sh4 = lrf[[2]]$t, sh5 = lrf[[5]]$t, spinks = lrf[[5]]$t)
lrf.c <- melt(lrf.comp)
lrf.res <- with(lrf.c, {
                pairwise.t.test(value, Var2)})
lrf.tab <- lrf.res$p.value
lrf.tab <- rbind('1' = rep(NA, 5), lrf.tab)
lrf.tab <- cbind(lrf.tab, '6' = rep(NA, 6))
#colnames(lrf.tab) <- coln
#rownames(lrf.tab) <- rown
lrf.tabx <- xtable(as.table(lrf.tab))
align(lrf.tabx) <- 'r|rrrrrr'
print.xtable(lrf.tabx, 
             file = '../doc/lrf_tabx.tex',
             hline.after = c(0, nrow(mm.tabx)),
             include.rownames = TRUE,
             floating = FALSE)
