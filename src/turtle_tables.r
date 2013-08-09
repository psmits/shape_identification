require(xtable)
require(reshape2)

load('../data/turtle_analysis.RData')
load('../data/turtle_gen.RData')

# 2 x 2 table of sex - cluster assignment
csex.tab <- rbind(csex.tab, tot = colSums(csex.tab))
csex.tab <- cbind(csex.tab, tot = rowSums(csex.tab))
xsex.tab <- xtable(csex.tab)
align(xsex.tab) <- 'r|cc|c'
digits(xsex.tab) <- c(0, 0, 0, 0)


# multinomial logistic regression model selection tables

cln <- colnames(tm.a.analysis$sel$spinks)
#cln[which(cln == 'delta')] <- "$\\delta AICc"
#cln[which(cln == 'weight')] <- "AICc weights"
rms <- which(cln == 'family')
tmcl <- lapply(tm.a.analysis$sel, function(x) {
               x <- x[, -rms]
               x})
cln <- cln[-rms]
tmtabs <- lapply(tmcl, function(x) {
                 colnames(x) <- cln
                 x})
tmx <- lapply(tmtabs, xtable)
tmx.1 <- tmx[[1]]


tmx.2 <- tmx[[2]]
tmx.3 <- tmx[[3]]
tmx.4 <- tmx[[4]]

#generalize comparisons
coln <- rown <- c('morph 1', 'morph 2', 'molec 1', 'molec 2')
mm.comp <- cbind(sh1 = mm$sh1$t, sh2 = mm$sh2$t, sh3 = mm$sh3$t, spinks = mm$spinks$t)
mm.c <- melt(mm.comp)
mm.res <- with(mm.c, {
               pairwise.wilcox.test(value, X2)})
# convert to table by adding a row and a column
mm.tab <- mm.res$p.value
mm.tab <- rbind('1' = rep(NA, 3), mm.tab)
mm.tab <- cbind(mm.tab, '4' = rep(NA, 4))
colnames(mm.tab) <- coln
rownames(mm.tab) <- rown
mm.tabx <- xtable(as.table(mm.tab))
align(mm.tabx) <- 'r|rrrr'

rf.comp <- cbind(sh1 = rr$sh1$t, sh2 = rr$sh2$t, sh3 = rr$sh3$t, spinks = rr$spinks$t)
rf.c <- melt(rf.comp)
rf.res <- with(rf.c, {
               pairwise.wilcox.test(value, X2)})
rf.tab <- rf.res$p.value
rf.tab <- rbind('1' = rep(NA, 3), rf.tab)
rf.tab <- cbind(rf.tab, '4' = rep(NA, 4))
colnames(rf.tab) <- coln
rownames(rf.tab) <- rown
rf.tabx <- xtable(as.table(rf.tab))
align(rf.tabx) <- 'r|rrrr'

lda.comp <- cbind(sh1 = ll[[1]]$t, sh2 = ll[[2]]$t, 
                  sh3 = ll[[3]]$t, spinks = ll[[4]]$t)
lda.c <- melt(lda.comp)
lda.res <- with(lda.c, {
                pairwise.wilcox.test(value, X2)})
lda.tab <- lda.res$p.value
lda.tab <- rbind('1' = rep(NA, 3), lda.tab)
lda.tab <- cbind(lda.tab, '4' = rep(NA, 4))
colnames(lda.tab) <- coln
rownames(lda.tab) <- rown
lda.tabx <- xtable(as.table(lda.tab))
align(lda.tabx) <- 'r|rrrr'
