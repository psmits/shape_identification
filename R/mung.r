##  data munging
##
##  peter d smits
##  psmits@uchicago.edu
###############################################################################

# packages
require(shapes)
require(geomorph)

# source files
source('../R/array2df.r')
source('../R/df2array.r')
source('../R/shape_distance.r')

# landmarks
n.land <- 26
n.dim <- 2
rawturt <- read.table('../data/raw_marmorata.txt')
land <- read.table('../data/marmorata_land.txt', sep = ' ')
land <- df2array(land, n.land, n.dim)
#cent <- lapply(land, 
#                      function(x) x[seq(from = n.land + 1, to = length(x))])

# meta data
meta <- read.csv('../data/marmorata_meta2.csv')
meta[meta == "?"] <- NA
meta[meta == "<NA>"] <- NA
names(meta) <- c('ind', 'spec', 'lat', 'long', 'year',
                 'sh1', 'sh2', 'sh3', 'spinks.pre', 'spinks',
                 'p.sex', 'b.sex', 'sex.pre')
meta$spinks <- as.factor(meta$spinks)

#  with no geographic infomation
no.geo <- is.na(meta$lat) | is.na(meta$long)
no.assign <- is.na(meta$sh1) | is.na(meta$sh2) |
is.na(meta$sh3) | is.na(meta$spinks)
no.info <- no.geo | no.assign
# remove
land.info <- land[, , !no.info]
meta.info <- meta[!no.info, ]
rawturt <- rawturt[!no.info, ]

geo <- cbind(lat = as.numeric(as.character(meta.info$lat)),
             long = (as.numeric(as.character(meta.info$long)) * -1))
outlier <- which(geo[, 'long'] > 0)
land.info <- land.info[, , -outlier]
meta.info <- meta.info[-outlier, ]
geo <- geo[-outlier, ]
rawturt <- rawturt[-outlier, ]

meta.info$lat <- geo[, 'lat']
meta.info$long <- geo[, 'long']

no.class <- which(meta.info$sh3 == '')
land.info <- land.info[, , -no.class]
meta.info <- meta.info[-no.class, ]
geo <- geo[-no.class, ]
rawturt <- rawturt[-no.class, ]

meta.info$sh1 <- as.factor(as.character(meta.info$sh1))
meta.info$sh2 <- as.factor(as.character(meta.info$sh2))
meta.info$sh3 <- as.factor(as.character(meta.info$sh3))
meta.info$spinks <- as.factor(as.character(meta.info$spinks))


# remove juvies
# there are two classification options. ASK KEN WHICH IS THE ONE TO FOLLOW
p.juv <- grep(pattern = '[jhJ]', 
              x = as.character(meta.info$p.sex), 
              perl = TRUE)
b.juv <- grep(pattern = '[jhJ]', 
              x = as.character(meta.info$b.sex),
              perl = TRUE)
land.adult <- land.info[, , -p.juv]
meta.adult <- meta.info[-p.juv, ]
geo.adult <- geo[-p.juv, ]
rawturt <- rawturt[-p.juv, ]

fit <- procGPA(land.adult)
adult.dist <- riem.matrix(land.adult)

adult <- cbind(data.frame(fit$stdscores),
               meta.adult)
