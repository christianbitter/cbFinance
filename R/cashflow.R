#'@author christian bitter
#'@title Cashflows
#'@name future_value
#'@param cashflow a collection of cashflows made to an idealized bank - a cashflow stream (positive and negative)
#'@param r interest rate (in percent)
#'@param terminal_period after how many future periods the cashflow should be realized. if this is zero, then
#'the terminal period is the last time step a cashflow transaction occurs (e.g., length(cashflow)).
#'@param simple Use linear (default) or exponential discounting
#'@param verbose should verbose logging be used
#'@description computes the future value of some cashflow stream using the provided interest rate,
#'cashflow transaction, compounding type and termin period.
#'@return returns the value of the cashflow after the terminal_period
#'@example
#'future_value(cashflow = cflow_stream, r = .1, terminal_period = 0);
#'@export
future_value <- function(cashflow = NULL, r = NULL, terminal_period = NULL, simple = T, verbose = T) {
  if (is.null(cashflow)) stop("future_value - cashflow stream not provided");
  if (is.null(r)) stop("future_value - interest rate r not provided");
  if (is.null(terminal_period)) stop("future_value - terminal period not provided");

  if (r < 0) stop("future_value - r cannot be negative");
  if (terminal_period < 0) stop("future_value - terminal period cannot be negative (0, to N)");

  cv <- 0;
  if (terminal_period == 0) terminal_period <- length(cashflow);
  if (terminal_period < length(cashflow)) cat("compounding stops before all cashflows are realized\r\n");

  for (l in 1:terminal_period) {
    cv <- cv + compound(A = cashflow[l], r = r, n = terminal_period - l, simple = simple);
  }

  return(cv);
}


#'@author christian bitter
#'@title Cashflows
#'@name present_value
#'@param cashflow a collection of cashflows made to an idealized bank - a cashflow stream (positive and negative)
#'@param r interest rate (in percent)
#'@param terminal_period after how many future periods the cashflow should be realized. if this is zero, then
#'the terminal period is the last time step a cashflow transaction occurs (e.g., length(cashflow)).
#'@param simple Use linear (default) or exponential discounting
#'@param verbose should verbose logging be used
#'@description computes the present value of some cashflow stream using the provided interest rate,
#'cashflow transaction, compounding type and termin period.
#'@return returns the value of the cashflow at period 0
#'@example
#'present_value(cashflow = cflow_stream, r = .1, terminal_period = 0, simple = F);
#'@export
present_value <- function(cashflow = NULL, r = NULL, terminal_period = NULL, simple = F, verbose = T) {
  if (is.null(cashflow)) stop("present_value - cashflow stream not provided");
  if (is.null(r)) stop("present_value - interest rate r not provided");
  if (is.null(terminal_period)) stop("present_value - terminal period not provided");

  if (r < 0) stop("present_value - r cannot be negative");
  if (terminal_period < 0) stop("present_value - terminal period cannot be negative (0, to N)");

  cv <- cashflow[1];

  if (terminal_period == 0) terminal_period <- length(cashflow);
  if (terminal_period < length(cashflow)) cat("compounding stops before all cashflows are realized\r\n");

  for (l in 2:terminal_period) {
    cv <- cv + discount(A = cashflow[l], r = r, n = l - 1, simple = simple);
  }

  return(cv);
}

