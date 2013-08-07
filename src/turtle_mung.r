###############################################################################
##
##  turtle data munging
##
##  peter d smits
##  psmits@uchicago.edu
##
###############################################################################

# packages
require(shapes)
require(geomorph)
#require(abind)

# source files
source('../src/support_functions.r')

# landmarks
n.land <- 26
n.dim <- 2
turtle.land <- read.table('../data/marmorata_land.txt', sep = ' ')
turtle.land <- as.list(as.data.frame(t(turtle.land)))
turtle.cent <- lapply(turtle.land, 
                      function(x) x[seq(from = n.land + 1, to = length(x))])
turtle.land <- lapply(turtle.land,
                      function(x) x[seq(n.land)])
turtle.land <- lapply(turtle.land,
                      function(x) t(matrix(x, nrow = n.dim)))
turtle.land <- Reduce(shapes::abind, turtle.land)

# meta data
turtle.meta <- read.csv('../data/marmorata_meta2.csv')
turtle.meta[turtle.meta == "?"] <- NA
turtle.meta[turtle.meta == "<NA>"] <- NA
names(turtle.meta) <- c('ind', 'spec', 'lat', 'long', 'year',
                        'sh1', 'sh2', 'sh3', 'spinks.pre', 'spinks',
                        'p.sex', 'b.sex', 'sex.pre')
turtle.meta$spinks <- as.factor(turtle.meta$spinks)

# turtles with no geographic infomation
no.geo <- is.na(turtle.meta$lat) | is.na(turtle.meta$long)
no.assign <- is.na(turtle.meta$sh1) | is.na(turtle.meta$sh2) |
             is.na(turtle.meta$sh3) | is.na(turtle.meta$spinks)
no.info <- no.geo | no.assign
# remove
turtle.land.info <- turtle.land[, , !no.info]
turtle.meta.info <- turtle.meta[!no.info, ]

turtle.geo <- cbind(lat = as.numeric(as.character(turtle.meta.info$lat)),
                    long = (as.numeric(as.character(turtle.meta.info$long)) * -1))
outlier <- which(turtle.geo[, 'long'] > 0)
turtle.land.info <- turtle.land.info[, , -outlier]
turtle.meta.info <- turtle.meta.info[-outlier, ]
turtle.geo <- turtle.geo[-outlier, ]

turtle.meta.info$lat <- turtle.geo[, 'lat']
turtle.meta.info$long <- turtle.geo[, 'long']

no.class <- which(turtle.meta.info$sh3 == '')
turtle.land.info <- turtle.land.info[, , -no.class]
turtle.meta.info <- turtle.meta.info[-no.class, ]
turtle.geo <- turtle.geo[-no.class, ]

turtle.meta.info$sh1 <- as.factor(as.character(turtle.meta.info$sh1))
turtle.meta.info$sh2 <- as.factor(as.character(turtle.meta.info$sh2))
turtle.meta.info$sh3 <- as.factor(as.character(turtle.meta.info$sh3))
turtle.meta.info$spinks <- as.factor(as.character(turtle.meta.info$spinks))




# remove juvies
# there are two classification options. ASK KEN WHICH IS THE ONE TO FOLLOW
p.juv <- grep(pattern = '[jhJ]', 
              x = as.character(turtle.meta.info$p.sex), 
              perl = TRUE)
b.juv <- grep(pattern = '[jhJ]', 
              x = as.character(turtle.meta.info$b.sex),
              perl = TRUE)
turtle.land.adult <- turtle.land.info[, , -p.juv]
turtle.meta.adult <- turtle.meta.info[-p.juv, ]
turtle.geo.adult <- turtle.geo[-p.juv, ]

turtle.fit <- procGPA(turtle.land.adult)
turtle.adult.dist <- riem.matrix(turtle.land.adult)

turtle.adult <- cbind(data.frame(turtle.fit$stdscores),
                      turtle.meta.adult)
