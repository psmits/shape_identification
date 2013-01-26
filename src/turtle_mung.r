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
require(abind)

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

turtle.fit <- procGPA(turtle.land)

turtle.dist <- riem.matrix(turtle.fit$rotated)  # riemmanian shape-distance


# meta data
turtle.meta <- read.csv('../data/marmorata_meta2.csv')
turtle.meta[turtle.meta == "?"] <- NA
turtle.meta[turtle.meta == "<NA>"] <- NA
names(turtle.meta) <- c('ind', 'spec', 'lat', 'long', 'year',
                        'sh1', 'sh2', 'sh3', 'spinks.pre', 'spinks',
                        'p.sex', 'b.sex', 'sex.pre')


# seperate turtles that have no geographic and/or spinks values
turtle.which.noinfo <- is.na(turtle.meta$lat) | is.na(turtle.meta$long) |
                 is.na(turtle.meta$spinks)
turtle.land.noinfo <- turtle.fit$rotated[, , turtle.which.noinfo]
turtle.land.info <- turtle.fit$rotated[, , !turtle.which.noinfo]
turtle.scores.noinfo <- turtle.fit$stdscores[turtle.which.noinfo, ]
turtle.scores.info <- turtle.fit$stdscores[!turtle.which.noinfo, ]

turtle.meta.info <- turtle.meta[!turtle.which.noinfo, ]
turtle.geo <- cbind(as.numeric(as.character(turtle.meta.info$lat)),
                    as.numeric(as.character(turtle.meta.info$long)))

