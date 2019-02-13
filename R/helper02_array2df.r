# make a nice data frame from a 3-dimenstional array
#
#  Args:
#    x: 3-way array
#
#  Returns:
#    data frame that is useful with ggplot
land.frame <- function(x) {
  m <- dim(x)[2]
  n <- dim(x)[3]
  dat <- x[, , 1]
  for (ii in seq(from = 2, to = n)) {
    dat <- rbind(dat, x[, , ii])
  }

  dat <- as.data.frame(dat)
  if (m == 2) nam <- c('x', 'y') else if (m == 3) nam <- c('x', 'y', 'z')
  names(dat) <- nam

  dat
}

