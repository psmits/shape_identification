resample.class <- function(data, class) {
  data[, class] <- sample(data[, class])
  data
}
