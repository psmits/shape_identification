## @knitr jitter
jitter.landmark <- function(land) {
  # randomly move landmarks in 2 (or 3) dimensions
  #
  #  Args:
  #    land: matrix with 2 (or 3) columns for x, y(,and z)
  #    factor:
  #    amount: maximum amount of noise
  #
  #  Returns:
  #    "jittered" landmarks
  
  for (ii in seq(ncol(land))) {
    land[, ii] <- land[, ii] + runif(n= length(land[, ii]), 
                                     min= -(min(abs(diff(land[, ii])))),
                                     max= min(abs(diff(land[, ii]))))
  }

  land
}

## @knitr fit-riem
# fit.riem <- function(land) {
#   # fit data and then find the riemannian distance matrix
#   #
#   #  Args:
#   #    land: landmark data in shapes format
#   #
#   #  Returns:
#   #   riemannian distance matrix
#   
#   # sdat <- procGPA(land)
#   # riem.comp <- riemdist(sdat$rotated[, , 1], sdat$rotate[, , 2])
#   riem.comp <- riemdist(land[, , 1], land[, , 2])
#   riem.comp
# }

## @knitr riem-dist
riem.distribution <- function(fit.land, nsim = 1000) {
  # create a distribution of riemannian distances
  #
  #  Args:
  #    fit.land: output from procGPA() function
  #    nsim: number of simulations
  #
  #  Returns:
  #    distribution of riemannian distances
  
  ran.land <- vector(mode= "list", length= nsim)
  for (ii in seq(nsim)) {
    s <- sample(fit.land$n, 2)
    sh1 <- jitter.landmark(fit.land$rotated[, , s[1]])
    sh2 <- jitter.landmark(fit.land$rotated[, , s[2]])
    ran.land[[ii]] <- abind(sh1, sh2, along= 3)
  }

  # currently set for parallel
  res <- mclapply(ran.land, function(x) {riemdis(x[, , 1], x[, , 2])}, mc.cores= detectCores(), mc.set.seed=T)
  
  res
}

## @knitr shape-sim
shape.simulate <- function(fit.land, nsim = 1000, probs = 0.50) {
  # do everything together to simulate a distrubtion 
  # of median riemannian distances
  #
  #  Args:
  #    land: output from procGPA
  #    nsim: number of simulations
  #    probs: cut off; default 0.05. interested in smaller than average values
  #           can be a vector of any length technically
  #
  #  Returns:
  #    cutoff value
  
  ri.di <- riem.distribution(fit.land, nsim=nsim)
  quantile(unlist(ri.di), probs = probs)
}




## old
#### OLD ####



# 
# # let's only do this with a single pair of landmarks
# fit.riem <- function(land) {
#   # fit data and then find the riemannian distance matrix
#   #
#   #  Args:
#   #    land: landmark data in shapes format
#   #
#   #  Returns:
#   #   riemannian distance matrix
#   
#   sdat <- procGPA(land)
#   riem.comp <- matrix(0, ncol = sdat$n, nrow = sdat$n)
#   samp <- sdat$n
#   for (jj in seq(samp)) for (kk in seq(samp)) {
#     riem.comp[jj, kk] <- 
#       riemdist(sdat$rotated[, , jj], sdat$rotated[, , kk])
#   }
#   riem.comp
# }
# 
# 
# riem.distribution <- function(land, factor = 1, amount = NULL, nsim = 1000) {
#   # create a distribution of riemannian distances
#   # try and find a way to make this parallel if necessary
#   # parallel solution is to make all the jitters at once
#   # and then over that jitter set: GPA and riem
#   # this requires massive restructing of these functions but will improve
#   # performance dramatically for data sets larger than the gorillas
#   #
#   #  Args:
#   #    land: landmark information in shapes format
#   #    factor:
#   #    amount: maximum amount of noise
#   #    nsim: number of simulations
#   #
#   #  Returns:
#   #    distribution of riemannian distances
#   
#   ran.land <- vector(mode= "list", length= nsim)
#   for (kk in seq(nsim)) {
#     for (ii in seq(dim(land)[3])) {
#       land[, , ii] <- 
#         jitter.landmark(land[, , ii], factor= factor, amount= amount)
#     }
#     ran.land[[kk]] <- land
#   }
#   
#   # currently set for parallel
#   res <- mclapply(ran.land, fit.riem, mc.cores= detectCores())
#   
#   res
# }
# 
# riem.quant <- function(ri.di, probs = 0.05) {
#   # find a quantile cut off for the distribution of riemannian distances
#   #
#   #  Args:
#   #    ri.di: output from riem.distribution
#   #    probs: cut off; default 0.05. interested in smaller than average values
#   #
#   #  Returns:
#   #    cutoff value
#   
#   quantile(unlist(lapply(ri.di, median)), probs = probs)
# }
# 
# shape.simulate <- function(land, factor = 1, amount = NULL, 
#                            nsim = 1000, probs = 0.05) {
#   # do everything together to simulate a distrubtion 
#   # of median riemannian distances
#   #
#   #  Args:
#   #    land: landmark information in shapes format
#   #    factor:
#   #    amount: maximum amount of noise
#   #    nsim: number of simulations
#   #    probs: cut off; default 0.05. interested in smaller than average values
#   #
#   #  Returns:
#   #    cutoff value
#   
#   ri.di <- riem.distribution(land, factor, amount, nsim)
#   riem.quant(ri.di, probs)
# }
