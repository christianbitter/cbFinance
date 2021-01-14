#'@author christian bitter
#'@title Compounding and Discounting
#'@name compound
#'@param A initial amount
#'@param r interest rate (in percent)
#'@param n compounding steps, or compounding time horizon. For example, a yearly compounding would be n = 1, with
#'an interval of interval = 1. Wheras a bi-yearly compounding over 1 year would be n = 1, interval = 2;
#'@param simple Use linear (default) or exponential compounding
#'@param interval the compounding interval (integer number) dividing the compounding steps n. Default 1L
#'@param continuous should we use continuous compounding
#'@description compounds some initial invest A for some time steps n using the
#'interest rate r
#'@return returns the compounded volume
#'@example
#'compound(A = 100, r = 0.05, n = 1, interval = 1L, simple = T, continuous = F);
#'@export
compound <- function(A = NULL, r = NULL, n = NULL, interval = 1L, simple = T, continuous = F) {
  if (is.null(A)) stop("compound - A cannot be null");
  if (is.null(r)) stop("compound - r cannot be null");
  if (is.null(n)) stop("compound - n cannot be null");
  if (is.null(interval)) stop("compound - interval cannot be null");

  if (r < 0) stop("compound - r cannot be negative");
  if (n < 0) stop("compound - n cannot be negative");
  if (interval < 1) stop("compound - interval cannot be < 1");
  if (!is.integer(interval) & !is.numeric(interval)) stop("cannot determine compounding interval (should be integer)");

  if (A == 0) return(0);
  if (n == 0) return(A);
  if (r == 0) return(A);

  compounded <- 0;
  interval   <- as.integer(interval);

  k <- r;
  m <- n;
  if (interval != 1) {
    k <- r / interval;
    m <- n * interval;
  }

  if (!simple) {
    if (continuous) {
      compounded <- A * exp(r * n);
    } else {
      compounded <- A * (1 + k)^m;
    }
  }
  else {
    compounded <- (1 + k * m) * A;
  }

  return(compounded);
}

#'@author christian bitter
#'@title Compounding and Discounting
#'@name discount
#'@param A initial amount
#'@param r interest rate (in percent)
#'@param n discounting steps
#'@param simple Use linear (simple) or exponential (non-simple, default) discounting
#'@param interval the discounting interval (integer number) dividing the discounting steps n. Default 1L
#'@description discounts some initial invest A for some time steps n using the
#'interest rate r (to derive the discount factor d)
#'@return returns the discounted volume
#'@example
#'discount(A = 100, r = 0.05, n = 1, interval = 1L, simple = T);
#'@export
discount <- function(A = NULL, r = NULL, n = NULL, interval = 1L, simple = F) {
  if (is.null(A)) stop("discount - A cannot be null");
  if (is.null(r)) stop("discount - r cannot be null");
  if (is.null(n)) stop("discount - n cannot be null");
  if (is.null(interval)) stop("discount - interval cannot be null");

  if (r < 0) stop("discount - r cannot be negative");
  if (n < 0) stop("discount - n cannot be negative");
  if (interval < 1) stop("discount - interval cannot be < 1");
  if (!is.integer(interval) & !is.numeric(interval)) stop("discount - cannot determine discounting interval (should be integer)");

  if (A == 0) return(0);
  if (n == 0) return(A);
  if (r == 0) return(A);

  interval   <- as.integer(interval);
  discounted <- 0;

  k <- r;
  m <- n;
  if (interval != 1) {
    k <- r / interval;
    m <- n * interval;
  }

  if (!simple) {
    discounted <- A / (1 + k)^m;
  }
  else {
    discounted <- A / (1 + k * m);
  }

  return(discounted);
}

#'@author christian bitter
#'@title Compounding and Discounting
#'@name effective_rate
#'@description computes the effective rate for the provided nominal rate and compouding
#'frequency.
#'@param r nominal interest rate (in percent)
#'@param interval interval as 1/interval when compounding is performed
#'@param continuous should we use continuous compounding
#'@return the effective rate for the given nominal rate
#'@example
#'effective_rate(r = 0.08, interval = 1L, continuous = F);
#'@export
effective_rate <- function(r = NULL, interval = 1L, continuous = F) {
  if (is.null(r)) stop("effective_rate - r cannot be null");
  if (is.null(interval)) stop("effective_rate - interval cannot be null");

  if (r < 0) stop("effective_rate - r cannot be negative");
  if (interval < 1) stop("effective_rate - interval cannot be < 1");
  if (!is.integer(interval)) stop("effective_rate - compounding interval has to be an integer fraction of n (e.g., 1, 2, ...)");

  r_i <- r / interval;
  if (!continuous) {
    er <- (1 + r_i)^interval;
  } else {
    er <- exp(r);
  }
  return(er - 1.);
}