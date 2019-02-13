#' Data frame in to shape array
#'
#' @param df data frame
#' @param n.land numeric; 2x number of landmarks
#' @param n.dim numeric; number of dimensions
#' @return multi-way array for shapes package
df2array <- function(df, n.land, n.dim) {

  df <- as.list(as.data.frame(t(df)))
  df <- lapply(df,
               function(x) x[seq(n.land)])
  df <- lapply(df,
               function(x) t(matrix(x, nrow = n.dim)))
  df <- Reduce(shapes::abind, df)
  df
}

