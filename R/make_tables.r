# various tables necessary for the paper
require(xtable)
require(reshape2)

source('../R/plotting_functions.r')

load('../data/gen.RData') 
#load('../data/shape.RData')

# 2 x 2 table of sex - cluster assignment
#csex.tab <- rbind(csex.tab, tot = colSums(csex.tab))
#csex.tab <- cbind(csex.tab, tot = rowSums(csex.tab))
#xsex.tab <- xtable(csex.tab)
#align(xsex.tab) <- 'r|cc|c'
#digits(xsex.tab) <- c(0, 0, 0, 0)
#print.xtable(xsex.tab,
#             file = '../doc/xsex_tab.tex',
#             floating = FALSE,
#             hline.after = c(0, nrow(csex.tab) - 1))


#generalize comparisons
#coln <- rown <- c('morph 1', 'morph 2', 'molec 1', 'molec 2')
mm.comp <- cbind(sh1 = mm$sh1$t, sh2 = mm$sh2$t, sh3 = mm$sh3$t, 
                 sh4 = mm$sh4$t, sh5 = mm$sh5$t, spinks = mm$spinks$t)
mm.c <- melt(mm.comp)
mm.res <- with(mm.c, {
               pairwise.wilcox.test(value, Var2)})

# convert to table by adding a row and a column
# multinomial logistic regression
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
               pairwise.wilcox.test(value, Var2)})
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
                pairwise.wilcox.test(value, Var2)})
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
                pairwise.wilcox.test(value, Var2)})
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
