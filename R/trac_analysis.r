library(shapes)
library(MuMIn)
library(geomorph)
library(plyr)
library(stringr)
library(reshape2)
library(cluster)
library(MASS)
library(nnet)
library(randomForest)
library(caret)
library(parallel)
library(doParallel)
library(xtable)
library(boot)
library(pROC)
library(ggplot2)
library(grid)
library(scales)
# source files
source('../R/array2df.r')
source('../R/df2array.r')
source('../R/shape_distance.r')
source('../R/caret_funcs.r')  # helper functions for train
source('../R/support_functions.r')
source('../R/multi_funcs.r')
source('../R/multiclass_roc.r')

# plot settings
theme_set(theme_bw())
cbp <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', 
         '#0072B2', '#D55E00', '#CC79A7')
theme_update(axis.text = element_text(size = 20),
             axis.title = element_text(size = 30),
             legend.text = element_text(size = 25),
             legend.title = element_text(size = 26),
             legend.key.size = unit(2, 'cm'),
             strip.text = element_text(size = 20))

# actually start doing analysis
trac <- list.files('../data/trach', pattern = 'txt', full.names = TRUE)
