# various tables necessary for the paper
library(xtable)
library(reshape2)
library(plyr)
library(boot)

source('../R/plotting_functions.r')
source('../R/miss_class.r')

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
# is there a significant difference between the "best" scheme and the others?

diff.table <- function(boot.mod, best, dif) {
  comp <- names(boot.mod)[best]
  means <- llply(boot.mod, function(x) as.numeric(x$t0))
  wbest <- names(means) == comp
  ccoo <- wbest
  ccoo[!wbest] <- dif
  ccoo[wbest] <- NA
  out <- cbind(scheme = names(means), compare = unlist(ccoo))
  xtable(out)
}

mmdif.tab <- diff.table(mm, best.mm, mm.dif)
label(mmdif.tab) <- 'mmdif'
print.xtable(x = mmdif.tab,
             file = '../doc/mm_dif_raw.tex')
rfdif.tab <- diff.table(rr, best.rr, rr.dif)
label(mmdif.tab) <- 'rfdif'
print.xtable(x = rfdif.tab,
             file = '../doc/rf_dif_raw.tex')
lldif.tab <- diff.table(ll, best.ll, ll.dif)
label(lldif.tab) <- 'lldif'
print.xtable(x = lldif.tab,
             file = '../doc/ll_dif_raw.tex')


# miss matches
sig.table <- function(test, groups) {
  sig.val <- lapply(test, function(x) {
                    x <- lapply(x, function(y) y[c(1,3)])
                    x[names(x) != 'rm']})
  row.nam <- llply(sig.val, names)
  sig.tab <- llply(sig.val, function(x) Reduce(rbind, x))
  sig.tab <- Map(function(x, y) {
                 rownames(x) <- y
                 x}, x = sig.tab, y = row.nam)
  gr.nam <- unlist(Map(function(x, y) rep(y, nrow(x)), x = sig.tab, y = groups))
  sig.tab <- Reduce(rbind, sig.tab)
  sig.tab <- data.frame(apply(sig.tab, 2, unlist))
  sig.tab$cat <- unlist(row.nam)
  sig.tab$gr <- gr.nam
  sig.tab <- sig.tab[, c(4, 3, 1, 2)]
  sig.tab <- xtable(sig.tab)
}
rfmiss.tab <- sig.table(rf.test, groups)
label(rfmiss.tab) <- 'rfmiss'
print.xtable(x = rfmiss.tab,
             file = '../doc/rf_miss_raw.tex')
mmmiss.tab <- sig.table(mm.test, groups)
label(mmmiss.tab) <- 'mmmiss'
print.xtable(x = mmmiss.tab,
             file = '../doc/mm_miss_raw.tex')
llmiss.tab <- sig.table(ll.test, groups)
label(llmiss.tab) <- 'llmiss'
print.xtable(x = llmiss.tab,
             file = '../doc/ll_miss_raw.tex')
