# prepare testing, training datasets and formulas for supervised analysis
#
# peter d smits
# psmits@uchicago.edu
###############################################################################
source('../R/support_functions.r')
source('../R/mung.r')

set.seed(1)

# formulas
max.ad <- nrow(adult) / 50
tvar.a <- paste('PC', 1:max.ad, sep = '')
groups <- list('sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks')
tform.a <- lapply(groups, function(x, y) make.form(y, x), y = tvar.a)

# training and testing data sets
set.seed(1)
ad.train <- data.maker(unlist(groups), adult)
adult.train <- lapply(ad.train, function(x) adult[x, ])
adult.test <- lapply(ad.train, function(x) adult[-x, ])

ad.class <- Map(function(x, y) colnames(x) %in% y, adult.test, groups)
ad.class <- Map(function(x, y) x[, y], adult.test, ad.class)

# design matrix
adult.design <- Map(function(x, y) {
                    cbind(lat = x$lat, long = x$long,
                          x[, 1:max.ad],
                          cate = x[, y])},
                    x = adult.train, y = groups)
