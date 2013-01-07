###############################################################################
##
##  data analysis
##
##  ben frable
##  peter d smits
##
###############################################################################

## libraries
require(MASS)
require(shapes)
require(geomorph)
require(cluster)
require(nnet)
require(caret)
require(e1071)
require(AICc)

## source files
source('../src/support_functions.r')
source('../src/fish_mung.r')
## important objects
##    fits: list of ano, cur, and all fish procGPA results
##    dists: list of ano, cur, and all fish riemmanian distance matrices

set.seed(1)

## machine learnings methods
## this needs to be updated with a randomized training set and test set
max.var <- floor(dim(fits$fish$scores)[1] / 10)
fish.group <- c(rep('ano', dim(ano.land)[3]), 
                rep('cur', dim(cur.land)[3]))
fish.data <- cbind(as.data.frame(fits$fish$stdscores),
                   group = fish.group)
in.train <- createDataPartition(fish.data$group, p = 0.75, list = FALSE)
fish.train <- fish.data[in.train, ]
fish.test <- fish.data[-in.train, ]


## linear discriminate analysis of eigenscores
## change this to just work with train....?
lin.dis <- lda(as.factor(fish.group) ~ fits$fish$stdscores[, seq(max.var)], 
               CV = TRUE, 
               method = 'mle')
lin.tab <- table(fish.group, lin.dis$class)
lin.group.accur <- diag(prop.table(lin.tab, 1))
lin.tot.accur <- sum(diag(prop.table(lin.tab)))

## quadratic discriminate analysis
quad.dis <- qda(as.factor(fish.group) ~ fits$fish$stdscores[, seq(max.var)], 
                CV = TRUE, 
                method = 'mle')
quad.tab <- table(fish.group, lin.dis$class)
quad.group.accur <- diag(prop.table(quad.tab, 1))
quad.tot.accur <- sum(diag(prop.table(quad.tab)))


## (multinomial) logistic/probit regression
## use 10-fold cross-validation
ctrl <- trainControl(method = 'LOOCV',
                     classProbs = TRUE,
                     number = 10)
mod <- train(group ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6,
             data = fish.train,
             method = 'multinom',
             trControl = ctrl,
             preProc = c('center', 'scale'))
pred.class <- predict(mod, fish.test, type = 'raw')
pred.accur <- postResample(pred.class, fish.test$group)
pred.confusion <- confusionMatrix(pred.class, fish.test$group)
pred.prob <- predict(mod, fish.test, type = 'prob')

## clustering
clust <- lapply(dists, clu)
