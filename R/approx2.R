approx2 <- function(x, y, new_x) {
  assertthat::assert_that(
    length(x) == length(y),
    length(x) > 1
  )
  y <- y[order(x)]
  x <- x[order(x)]
  n <- length(x)
  ifelse(
    new_x < x[1],
    y[1] + (new_x - x[1]) * (y[2] - y[1]) / (x[2] - x[1]),
    ifelse(
      new_x > x[n],
      y[n] + (new_x - x[n]) * (y[n] - y[n - 1]) / (x[n] - x[n - 1]),
      approx(x, y, xout = new_x)$y
    )
  )
}
