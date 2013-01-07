###############################################################################
##
##  analysis of brain data
##
##  peter d smits
##
###############################################################################

## libraries
require(shapes)
require(geomorph)
require(nnet)
require(caret)
require(e1071)
require(MuMIn)

## source files
source('../src/support_functions.r')

## seed
set.seed(1)


## ctrl sets
ctrl <- trainControl(method = 'LOOCV',
                     classProbs = TRUE,
                     number = 10)


## analysis
data(brains)
brain <- procGPA(brains$x)

brain.group <- brains$grp

brain.data <- cbind(as.data.frame(brain$stdscores),
                    group = as.factor(brain.group))

brain.in.train <- createDataPartition(brain.data$group, p = 0.75, list = FALSE)
brain.train <- brain.data[brain.in.train, ]
brain.test <- brain.data[-brain.in.train, ]

## multinomial
brain.mod <- train(group ~ PC1,
                   data = brain.train,
                   method = 'multinom')
brain.pred.class <- predict(brain.mod, brain.test$group)
brain.pred.accur <- postResample(brain.pred.class, brain.test$group)
brain.pred.confusion <- confusionMatrix(brain.pred.class, brain.test$group)
brain.pred.prob <- predict(brain.mod, brain.test$group, type = 'prob')
