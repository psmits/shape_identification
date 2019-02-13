# various functions involved with supervised learning via caret
library(pacman)

p_load(here, MASS, nnet, randomForest, caret)

source(here::here('R', 'helper05_multiclass_roc.r'))

ctrl <- trainControl(method = 'repeatedcv',
                     classProbs = TRUE,
                     summaryFunction = multiClassSummary,
                     number = 5,
                     repeats = 10,
                     returnResamp = 'all')
