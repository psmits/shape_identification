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
require(MuMIn)

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
                rep('cur', dim(cur.land)[3]),
                rep('pro', dim(pro.land)[3]),
                rep('chi', dim(chi.land)[3]))
fish.data <- cbind(as.data.frame(fits$fish$stdscores),
                   group = fish.group)
in.train <- createDataPartition(fish.data$group, p = 0.75, list = FALSE)
fish.train <- fish.data[in.train, ]
fish.test <- fish.data[-in.train, ]


## (multinomial) logistic/probit regression
## use 10-fold cross-validation
ctrl <- trainControl(method = 'LOOCV',
                     classProbs = TRUE,
                     number = 10)
fish.formula <- vector(mode = 'list', length = max.var)
fish.vars <- paste('PC', 1:max.var, sep = '')
for (ii in seq(max.var)) {
  fish.formula[[ii]] <- as.formula(paste('group ~ ', 
                                         paste(fish.vars[seq(ii)], 
                                               collapse= '+')))
}

fish.mod <- lapply(fish.formula, 
                   train, 
                   data = fish.train, 
                   method = 'multinom', 
#                   trControl = ctrl, 
                   preProc = c('center', 'scale'))
fish.model.table <- model.sel(lapply(fish.mod,
                                     function(x) x$finalModel))
fish.avg.model <- model.avg(lapply(fish.mod, 
                                   function(x) x$finalModel))
best.fish.mod <- fish.mod[[as.numeric(rownames(fish.model.table)[1])]]
fish.pred.class <- predict(best.fish.mod, fish.test, type = 'raw')
fish.pred.accur <- postResample(fish.pred.class, fish.test$group)
fish.pred.confusion <- confusionMatrix(fish.pred.class, fish.test$group)
fish.pred.prob <- predict(best.fish.mod, fish.test, type = 'prob')

## clustering
clust <- lapply(dists, clu)
