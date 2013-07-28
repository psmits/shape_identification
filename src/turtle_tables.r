require(xtable)

load('../data/turtle_analysis.RData')
load('../data/turtle_gen.RData')


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
