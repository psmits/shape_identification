library(shapes)
library(geomorph)
library(cluster)

source('../src/turtle_mung.r')

load('../data/cluster_res.RData')

# correlation between size and the first couple PCs
cs <- centroid.size(turtle.land.adult)
ipc <- turtle.adult[, 1:2]

scor <- apply(ipc, 2, function(x) cor.test(cs, x))


# sexual dimorphism in the clustering solution
set.seed(1)
gap.second <- pam(as.dist(turtle.adult.dist), k = 2)
tclus <- gap.second$clustering

tsex <- as.character(turtle.adult$p.sex)  # lots of regex to get this sexy
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

save(tsex, tclus.s, tsex.s, csex.tab, csex.chi,
     file = '../data/turtle_shape.RData')
