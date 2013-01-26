###############################################################################
##
##  analysis of turtle plastron data
##
##  peter d smits
##  psmits@uchicago.edu
##
###############################################################################

# libraries
require(MASS)
require(nnet)
require(cluster)
require(e1071)
require(caret)
require(MuMIn)

# source files
source('../src/support_functions.r')
source('../src/turtle_mung.r')

set.seed(1)

# how many groups are there just from the geo reference info 
n.groups <- max(turtle.meta$spinks, na.rm = TRUE)
turtle.km <- kmeans(na.omit(turtle.geo), k = n.group) 
turtle.fuzzy <- cmeans(na.omit(turtle.geo), centers = n.groups)

