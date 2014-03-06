## @knitr fish-set-up
require(shapes)
require(igraph)
require(parallel)
require(abind)
require(cluster)
require(pvclust)
source('landmark_montecarlo.R')
source('/home/peter/Documents/projects/3294254/readtps.R')
RNGkind("L'Ecuyer-CMRG")
set.seed(1)


ano <- read.tps("AnostomidaeFinal.TPS", shapes= TRUE)
cur <- read.tps("CurimatidaeF.TPS", shapes= TRUE)
fish <- abind(ano$landmark, cur$landmark)
fish <- fish[, , -42]

## @knitr fish-fit
fish.fit <- procGPA(fish)

riem.comp <- matrix(0, ncol = fish.fit$n, nrow = fish.fit$n)
samp <- fish.fit$n
for (ii in seq(samp)) for (kk in seq(samp)) {
  riem.comp[ii, kk] <- 
    riemdist(fish.fit$rotated[, , ii], fish.fit$rotated[, , kk])
}

# basic clustering
# hierarchical of the riemmanian distances
fish.ha <- agnes(as.dist(riem.comp), method = 'ward')

# divisive clustering of the riemmanian distances
fish.di <- diana(as.dist(riem.comp))

# note to self:
# i have to rewrite pvclust to serve my own purposes.


### @knitr fish-MC
#cutoff <- shape.simulate(fish.fit, nsim= 10, probs= 0.5)
#riem.comp[riem.comp > cutoff] <- 0
#
## invert so the higher weight means closer
#riem.comp[riem.comp != 0] <- 1/riem.comp[riem.comp != 0]
#
### @knitr fish-network
## start the network analysis
#data.graph <- graph.adjacency(riem.comp, mode = "undirected", weighted = TRUE)
#
## communities
#data.fc <- fastgreedy.community(data.graph)
#data.ebc <- edge.betweenness.community(data.graph)
#data.lec <- leading.eigenvector.community(data.graph)
#
### @knitr fish-plots
## if (is.hierarchical(data.fc)) {
##   data.hier <- as.hclust(data.fc)
## }
#
#E(data.graph)$width <- log(E(data.graph)$weight) / 5
## general plot
#colors <- c("goldenrod", "skyblue")  # families of fish
#vsh <- c("circle", "square", "csquare", "rectangle",
#         "crectangle", "vrectangle")
#plot(data.graph, 
#     # layout = 
#     mark.groups= communities(data.fc),
#     vertex.size = 6,
#     vertex.shape = vsh[data.fc$membership],
#     vertex.color = colors[c(rep(1, times = 18), rep(2, times = 49))],
#     vertex.label.cex = 0.5)
#
#plot(data.graph, 
#     # layout = 
#     mark.groups= communities(data.ebc),
#     vertex.size = 6,
#     vertex.color = colors[c(rep(1, times = 18), rep(2, times = 49))],
#     vertex.label.cex = 0.5)
#
#plot(data.graph, 
#     # layout = 
#     mark.groups= communities(data.lec),
#     vertex.size = 6,
#     vertex.shape = vsh[data.lec$membership],
#     vertex.color = colors[c(rep(1, times = 18), rep(2, times = 49))],
#     vertex.label.cex = 0.5)
#
## plot(minimum.spanning.tree(data.graph), mark.groups = communities(data.wtc))
