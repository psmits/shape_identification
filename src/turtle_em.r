###############################################################################
##
##  analysis of turtle plastron data
##  unsupervised learning approaches
##
##  peter d smits
##  psmits@uchicago.edu
##
###############################################################################

# libraries
require(mclust)
set.seed(1)


# source files
source('../src/support_functions.r')
source('../src/turtle_mung.r')

turtle.em <- Mclust(turtle.info[, 1:19])

#save(turtle.em, 'turtle_em_res.RData')
