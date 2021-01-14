#'@title Returns
#'@author Christian Bitter
#'@export
#'@name random_walk
random_walk <- function(x0, t) {
  if (t < 1) stop("t has to be > 0");

  Time <- sample(x = c(-1, 1), size = t, replace = T);
  X <- c(x0, Time)
  X <- cumsum(X)

  return(tibble::tibble("t" = 0:t, "x" = X, "e" = c(Time, 0)));
}