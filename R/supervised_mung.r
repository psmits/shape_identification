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
source('../R/support_functions.r')

# landmarks
n.land <- 26
n.dim <- 2
rawturt <- read.table('../data/raw_marmorata.txt')
land <- read.table('../data/marmorata_land.txt', sep = ' ')
land <- df2array(land, n.land, n.dim)
#centroid <- rawturt[, n.land + 1]  # size of each observation

# meta data
#meta <- read.csv('../data/marmorata_meta2.csv')
meta <- read.csv('../data/marm_upbins.csv', skip = 1, header = TRUE)
#names(meta) <- c('ind', 'spec', 'lat', 'long', 'year',
#                 'sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks.pre', 'spinks',
#                 'p.sex', 'b.sex', 'sex.pre', 'notes')
names(meta) <- c('ind', 'spec', 'lat', 'long', 'year',
                 'sp10.1', 'sp10.2', 'sp10.3', 'sp14.1', 'sp14.2', 'morph', 
                 'bad1', 'bad2', 'bad3',
                 'p.sex', 'b.sex', 'sex.pre', 'notes')
cl <- c('sp10.1', 'sp10.2', 'sp10.3', 'sp14.1', 'sp14.2', 'morph')
meta[, cl] <- apply(meta[, cl], 2, function(x) as.character(x))
meta[, c('lat', 'long')] <- apply(meta[, c('lat', 'long')], 
                                  2, function(x) as.numeric(as.character(x)))
meta[meta == '?' | meta == '??'] <- NA

# get rid of everything that doesn't have lat-long
#meta$long <- meta$long
land <- land[, , !is.na(meta$lat)]
rawturt<- rawturt[!is.na(meta$lat), ]
meta <- meta[!is.na(meta$lat), ]

# get rid of everything that doesn't have assigns
# clean off the missing important values

imp <- c('lat', 'long', 'sp10.1', 'sp10.2', 'sp10.3', 'sp14.1', 'sp14.2')
#rms <- Reduce('|', alply(meta[, imp], 2, is.na))
#land <- land[, , !rms]
#meta <- meta[!rms, ]
#rawturt <- rawturt[!rms, ]


# clean the juveniles
jv <- grepl(pattern = '[jJh]', meta$p.sex)
land.adult <- land[, , !jv]
meta.adult <- meta[!jv, ]
rawturt <- rawturt[!jv, ]

rms <- is.na(meta.adult$sp10.1) | meta.adult$sp10.1 == ''
land.adult <- land.adult[, , !rms]
meta.adult <- meta.adult[!rms, ]
rawturt <- rawturt[!rms, ]
centroid <- rawturt[, n.land + 1]  # size of each observation


geo <- cbind(lat = meta.adult$lat, long = meta.adult$long)

fit <- procGPA(land.adult)
#adult.dist <- riem.matrix(land.adult)

centroid <- scale(centroid)
adult <- cbind(data.frame(size = centroid, 
                          inter = centroid * fit$stdscores[, 1],
                          fit$stdscores),
               meta.adult)


schemes <- c('sp10.1', 'sp10.2', 'sp10.3', 'sp14.1', 'sp14.2', 'morph')
nspec <- aaply(meta.adult[, schemes], 2, function(x) {
                 n <- is.na(x) | x == ''
                 length(x[!n])})

cliped <- meta.adult[, c('spec', 'lat', 'long', 
                         'sp10.1', 'sp10.2', 'sp10.3', 
                         'sp14.1', 'sp14.2', 
                         'morph')]
write.csv(cliped, file = '../data/clean_meta_marm.csv', fileEncoding = 'UTF-16LE')
