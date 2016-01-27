##  data munging
##
##  peter d smits
##  psmits@uchicago.edu
###############################################################################

# packages
library(shapes)
library(geomorph)
library(plyr)

# source files
source('../R/array2df.r')
source('../R/df2array.r')
source('../R/shape_distance.r')
source('../R/support_functions.r')

# landmarks
n.land <- 26
n.dim <- 2
rawturt <- read.table('../data/raw_marmorata.txt')
land <- read.table('../data/marmorata_land.txt', sep = ' ')
land <- df2array(land, n.land, n.dim)
centroid <- rawturt[, n.land + 1]  # size of each observation

# meta data
#meta <- read.csv('../data/marmorata_meta2.csv')
meta <- read.csv('../data/turtle_update.csv')
meta[meta == "?"] <- NA
meta[meta == "<NA>"] <- NA
names(meta) <- c('ind', 'spec', 'lat', 'long', 'year',
                 'sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks.pre', 'spinks',
                 'p.sex', 'b.sex', 'sex.pre', 'notes')
meta$sh4 <- as.factor(meta$sh4)
meta$spinks <- as.factor(meta$spinks)

# clean off the missing important values
imp <- c('lat', 'long', 'sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks', 'p.sex')
rms <- Reduce('|', alply(meta[, imp], 2, is.na))
land <- land[, , !rms]
meta <- meta[!rms, ]
rawturt <- rawturt[!rms, ]

# clean the geographic information
meta[, c('lat', 'long')] <- apply(meta[, c('lat', 'long')], 
                                  2, function(x) as.numeric(as.character(x)))
meta$long <- meta$long * -1
meta$long[meta$long > 0] <- meta$long[meta$long > 0] * -1

cl <- c('sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks')
meta[, cl] <- apply(meta[, cl], 2, function(x) x)
meta$sh1 <- factor(meta$sh1)
meta$sh2 <- factor(meta$sh2)
meta$sh3 <- factor(meta$sh3)
meta$sh4 <- factor(meta$sh4)
meta$sh5 <- factor(meta$sh5)
meta$spinks <- factor(meta$spinks)

# clean the juveniles
jv <- grepl(pattern = '[jJh]', meta$p.sex)
land.adult <- land[, , !jv]
meta.adult <- meta[!jv, ]
rawturt <- rawturt[!jv, ]

geo <- cbind(lat = meta.adult$lat, long = meta.adult$long)

fit <- procGPA(land.adult)
#adult.dist <- riem.matrix(land.adult)

adult <- cbind(data.frame(fit$stdscores),
               meta.adult)

# ok, now make the formulas
# formulas
max.ad <- nrow(adult) / 10
max.ad <- min(c(max.ad, 26 - 3))
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
