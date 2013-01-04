###############################################################################
##
##  data analysis
##
##  ben frable
##  peter d smits
##
###############################################################################

## libraries
require(shapes)
require(geomorph)
require(cluster)
require(MASS)

## source files
source('support_functions.r')
source('fish_mung.r')
## important objects
##    fits: list of ano, cur, and all fish procGPA results
##    dists: list of ano, cur, and all fish riemmanian distance matrices


## machine learnings methods
## this needs to be updated with a randomized training set and test set

max.var <- floor(dim(fits$fish$scores)[1] / 10)
## linear discriminate analysis of eigenscores
fish.group <- c(rep('ano', dim(ano.land)[3]), rep('cur', dim(cur.land)[3]))
lin.dis <- lda(as.factor(fish.group) ~ fits$fish$scores[, seq(max.var)], 
               CV = TRUE, 
               method = 'mle')
lin.tab <- table(fish.group, lin.dis$class)
lin.group.accur <- diag(prop.table(lin.tab, 1))
lin.tot.accur <- sum(diag(prop.table(lin.tab)))

## quadratic discriminate analysis
quad.dis <- qda(as.factor(fish.group) ~ fits$fish$scores[, seq(max.var)], 
                CV = TRUE, 
                method = 'mle')
quad.tab <- table(fish.group, lin.dis$class)
quad.group.accur <- diag(prop.table(quad.tab, 1))
quad.tot.accur <- sum(diag(prop.table(quad.tab)))


## (multinomial) logistic/probit regression


## clustering
clust <- lapply(dists, clu)
