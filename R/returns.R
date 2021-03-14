#'@title Returns
#'@author Christian Bitter
#'@export
#'@name simple_return
simple_return <- function(prices) {
  if (is.na(prices)) stop("Missing prices")
  if (missing(prices)) stop("Missing prices")
  if (length(prices) < 1) stop("Missing prices")

  return(c(0, diff(prices)) / prices);
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
  return(c(0, diff(log(prices))));
}

#'@title Returns
#'@author Christian Bitter
#'@export
#'@name excess_return
excess_return <- function(log_return, log_return_riskfree) {
  return(log_return - log_return_riskfree);
}

#'@title Returns
#'@author Christian Bitter
#'@export
#'@name expected_return
expected_return <- function(log_return) {
  return(mean(log_return));
}

#'@title Returns
#'@author Christian Bitter
#'@export
#'@name expected_annual_return
expected_annual_return <- function(log_return, trading_days = 250) {
  return(trading_days * expected_return(log_return));
}

#'@title Returns
#'@author Christian Bitter
#'@export
#'@name risk_premium
risk_premium <- function(log_return, log_return_riskfree) {
  return(mean(excess_return(log_return, log_return_riskfree)));
}