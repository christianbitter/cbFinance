#'@title Returns
#'@author Christian Bitter
#'@export
#'@name simple_return
simple_return <- function(prices) {
  if (is.na(prices)) stop("Missing prices")
  if (missing(prices)) stop("Missing prices")
  if (length(prices) < 1) stop("Missing prices")

  N <- length(prices);
  x_i <- prices[1:N - 1];
  x_j <- prices[2:N]
  return((x_j - x_i) / x_i);
}

#'@title Returns
#'@author Christian Bitter
#'@export
#'@name arithmetic_return
arithmetic_return <- function(prices) {
  simple_returns <- simple_return(prices);
  return(sum(simple_returns));
}

#'@title Returns
#'@author Christian Bitter
#'@export
#'@name mean_arithmetic_return
mean_arithmetic_return <- function(prices) {
  arithmetic_returns <- arithmetic_return(prices);
  Ninv <- 1 / length(arithmetic_returns);
  return(Ninv * arithmetic_returns);
}

#'@title Returns
#'@author Christian Bitter
#'@export
#'@name geometric_return
geometric_return <- function(prices) {
  simple_returns <- simple_return(prices);
  R <- simple_returns + 1;
  p <- prod(R) - 1;
  return(p);
}

#'@title Returns
#'@author Christian Bitter
#'@export
#'@name geometric_mean_returns
geometric_mean_returns <- function(prices) {
  geometric_returns <- geometric_return(prices);
  Ninv <- 1. / length(geometric_returns);

  return(geometric_returns^Ninv)
}

#'@title Returns
#'@author Christian Bitter
#'@export
#'@name log_return
log_return <- function(prices) {
  N <- length(prices);
  x_i <- log(prices[1:N - 1]);
  x_j <- log(prices[2:N]);
  return(x_j - x_i);
}