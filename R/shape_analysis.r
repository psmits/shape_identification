library(shapes)
library(geomorph)
library(cluster)

source('../R/mung.r')

#load('../data/cluster_res.RData')


# correlation between size and the first couple PCs
cs <- rawturt[, ncol(rawturt)]
ipc <- adult[, 1:2]

scor <- apply(ipc, 2, function(x) cor.test(cs, x, method = 'spearman'))


# sexual dimorphism in the clustering solution
set.seed(1)
gap.second <- pam(as.dist(adult.dist), k = 2)
tclus <- gap.second$clustering

tsex <- as.character(adult$p.sex)  # lots of regex to get this sexy
fe <- grep(pattern = '[fF]', tsex, perl = TRUE)
ma <- grep(pattern = '[mM]', tsex, perl = TRUE)
na <- grep(pattern = '[nN]', tsex, perl = TRUE)
tsex[fe] <- 'F'
tsex[ma] <- 'M'
tsex[na] <- NA
tsex[tsex == ''] <- NA

sexrm <- which(is.na(tsex))
tclus.s <- tclus[-sexrm]
tsex.s <- tsex[-sexrm]
csex.tab <- table(tclus.s, tsex.s)
csex.chi <- chisq.test(csex.tab)

save(cs, scor, tsex, tclus.s, tsex.s, csex.tab, csex.chi,
     file = '../data/shape.RData')
