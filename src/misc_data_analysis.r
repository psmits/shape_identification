###############################################################################
##
##  miscellaneous morphometric data sets included in the packages
##  machine learning methods used here to test ideas
##
##  Peter D Smits
##
###############################################################################

## libraries
require(MASS)
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


## ctrl set(s)
ctrl <- trainControl(method = 'LOOCV',
                     classProbs = TRUE,
                     number = 10)


## primate vertebrae
monkey <- abind::abind(gorf.dat, gorm.dat, 
                       pongof.dat, pongom.dat, 
                       panf.dat, panm.dat)
mon <- procGPA(monkey)

mon.group <- c(rep('female.gor', dim(gorf.dat)[3]),
               rep('male.gor', dim(gorm.dat)[3]),
               ##  there is a potentially major problem with the gorilla data set
               ##  need to read the original paper that analyzes it
               rep('female.pongo', dim(pongof.dat)[3]),
               rep('male.pongo', dim(pongom.dat)[3]),
               rep('female.pan', dim(panf.dat)[3]),
               rep('male.pan', dim(panm.dat)[3]))
mon.species <- c(rep('gor', (dim(gorf.dat)[3] + dim(gorm.dat)[3])),
                 rep('pongo', (dim(pongof.dat)[3] + dim(pongom.dat)[3])),
                 rep('pan', (dim(panf.dat)[3] + dim(panm.dat)[3])))
mon.data <- cbind(as.data.frame(mon$stdscores),
                  group = mon.group, species = mon.species)

## mouse verteabrae data
mouse <- abind::abind(qcet2.dat, qlet2.dat, qset2.dat)
mouse <- procGPA(mouse)
mouse.group <- c(rep('control', dim(qcet2.dat)[3]),
                 rep('large', dim(qlet2.dat)[3]),
                 rep('small', dim(qset2.dat)[3]))
mouse.data <- cbind(as.data.frame(mouse$stdscores),
                    group = mouse.group)

## combine and make pretty
land.proc <- list(monkey = mon,
                  mouse = mouse)
land.data <- list(monkey = mon.data,
                  mouse = mouse.data)

land.in <- lapply(land.data, function(x) {
                  createDataPartition(x$group,
                  p = 0.75, list = FALSE)} )

land.train <- Map(function(x, y) {x[y, ]}, 
                  land.data,
                  land.in)

land.test <- Map(function(x, y) {x[-y, ]},
                 land.data,
                 land.in)


## multinomial
land.mod <- lapply(land.train, multi.mod, 
                   formula = group ~ PC1 + PC2 + PC3,
                   preProc = c('center', 'scale'))
land.pred.class <- Map(predict, land.mod, land.test)
land.pred.accur <- Map(function(x, y) {postResample(x, y$group)},
                       land.pred.class, land.test)
land.pred.confusion <- Map(function(x, y) {confusionMatrix(x, y$group)},
                           land.pred.class, land.test)
land.pred.prob <- Map(function(x, y) {predict(x, y, type = 'prob')},
                      land.mod, land.test)
