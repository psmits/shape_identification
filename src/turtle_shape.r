library(shapes)
library(geomorph)

source('../src/turtle_mung.r')

# correlation between size and the first couple PCs
cs <- centroid.size(turtle.land.adult)
ipc <- turtle.adult[, 1:2]

apply(ipc, 2, function(x) cor.test(cs, x))
