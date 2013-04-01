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


# meta data
turtle.meta <- read.csv('../data/marmorata_meta2.csv')
turtle.meta[turtle.meta == "?"] <- NA
turtle.meta[turtle.meta == "<NA>"] <- NA
names(turtle.meta) <- c('ind', 'spec', 'lat', 'long', 'year',
                        'sh1', 'sh2', 'sh3', 'spinks.pre', 'spinks',
                        'p.sex', 'b.sex', 'sex.pre')
turtle.meta$spinks <- as.factor(turtle.meta$spinks)


# seperate turtles that have no geographic and/or spinks values
turtle.which.noinfo <- is.na(turtle.meta$lat) | is.na(turtle.meta$long) |
                 is.na(turtle.meta$spinks) | is.na(turtle.meta$sh1) |
                 is.na(turtle.meta$sh2) | is.na(turtle.meta$sh3)
turtle.land.noinfo <- turtle.fit$rotated[, , turtle.which.noinfo]
turtle.land.info <- turtle.fit$rotated[, , !turtle.which.noinfo]
turtle.scores.noinfo <- turtle.fit$stdscores[turtle.which.noinfo, ]
turtle.scores.info <- turtle.fit$stdscores[!turtle.which.noinfo, ]


turtle.meta.info <- turtle.meta[!turtle.which.noinfo, ]
turtle.geo <- cbind(as.numeric(as.character(turtle.meta.info$lat)),
                    as.numeric(as.character(turtle.meta.info$long)))
# there are two geographic outliers....need to check with other guys
turtle.geo.meta <- turtle.meta.info[(turtle.geo[, 2]) > 100, ]

turtle.land.info <- turtle.land.info[, , (turtle.geo[, 2]) > 100]

turtle.info <- cbind(turtle.scores.info[(turtle.geo[, 2]) > 100, ],
                                         turtle.geo.meta)

turtle.land.info <- turtle.land.info[, , turtle.info$sh3 != '']

turtle.info <- turtle.info[turtle.info$sh3 != '', ]
turtle.info$spinks <- as.factor(as.character(turtle.info$spinks))
turtle.info$sh1 <- as.factor(as.character(turtle.info$sh1))
turtle.info$sh2 <- as.factor(as.character(turtle.info$sh2))
turtle.info$sh3 <- as.factor(as.character(turtle.info$sh3))

turtle.info.dist <- riem.matrix(turtle.land.info)  # riemmanian shape-distance

turtle.geo <- turtle.geo[(turtle.geo[, 2]) > 100, ]

