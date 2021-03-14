#'@title Risk
#'@author Christian Bitter
#'@export
#'@name volatility
volatility <- function(log_returns) {
  return(sd(log_returns, na.rm = T))
}

#'@title Risk
#'@author Christian Bitter
#'@export
#'@name semideviation
semideviation <- function(log_returns) {
  muR <- mean(log_returns);
  rI  <- log_returns < muR;
  n_ <- sum(rI);
  dof <- n_ + 1;
  ssd <- sum((log_returns - muR)^2) / dof;
  return(ssd);
}

#'@title Risk
#'@author Christian Bitter
#'@export
#'@name expected_shortfall
expected_shortfall <- function(log_returns, alpha = 0.05) {
  v_alpha_r <- quantile(log_returns, probs = alpha);
  return(mean(log_returns[log_returns <= v_alpha_r]));
}

#'@title Risk
#'@author Christian Bitter
#'@export
#'@name longest_loss_period
longest_loss_period <- function(log_returns) {
  l <- length(log_returns);
  tmax <- 1;
  tmin <- 1;

  for (i in 1:(l - 1)) {
    if (log_returns[i] > log_returns[tmax]) {
      tmax <- i;
      tmin <- i;
    }
    if (log_returns[i + 1] < log_returns[tmin]) {
      tmin <- i + 1;
    }
  }

  return(c(tmax, tmin, tmax - tmin));
}

# https://de.extraetf.com/wissen/maximum-drawdown-definition

#'@title Risk
#'@author Christian Bitter
#'@export
#'@name max_drawdown
max_drawdown <- function(log_returns) {
  l <- length(log_returns);
  current_min <- log_returns[1];
  current_max <- log_returns[1];

  for (i in 1:(l - 1)) {
    if (log_returns[i] > current_max) {
      current_max <- log_returns[i];
      current_min <- current_max;
    }
    if (log_returns[i + 1] < current_min) {
      current_min <- log_returns[i + 1];
    }
  }

  return(current_max - current_min);
}

#'@title Risk
#'@author Christian Bitter
#'@export
#'@name value_at_risk
value_at_risk <- function(log_returns, alpha = .05){
  v_alpha_r <- quantile(x = log_returns, probs = alpha);
  return(v_alpha_r);
}

#'@title Risk
#'@author Christian Bitter
#'@export
#'@name annualized_volatility
annualized_volatility <- function(log_returns, trading_days = 250) {
  return(sqrt(trading_days) * volatility(log_returns));
}

#'@title Risk
#'@author Christian Bitter
#'@export
#'@name sharpe_ratio
sharpe_ratio <- function(log_returns, risk_free_return, trading_days=250) {
  exp_ret_a <- trading_days * mean(log_returns - risk_free_return);
  vol_a     <- annualized_volatility(log_returns, trading_days = trading_days);
  return(exp_ret / vol_anno);
}

#'@title Risk
#'@author Christian Bitter
#'@export
#'@name sortino_ratio
sortino_ratio <- function(log_returns, risk_free_return, trading_days) {
  exp_ret_a  <- trading_days * excess_return(log_returns, risk_free_return);
  vol_a      <- sqrt(trading_days) * semideviation(log_returns);
  return(exp_ret_a / vol_a);
}

#'@title Risk
#'@author Christian Bitter
#'@export
#'@name conditional_sharpe_ratio
conditional_sharpe_ratio <- function(log_returns, risk_free_return, trading_days = 250, alpha = .05) {
  exp_ret_a  <- trading_days * excess_return(log_return = log_returns, log_return_riskfree = risk_free_return);
  vol_a      <- sqrt(trading_days) * expected_shortfall(log_returns, alpha = alpha);
  return(exp_ret_a / vol_a);
}

Information_ratio <- function(log_returns_portfolio, log_returns_alternative) {
  # TODO:
  stop("TODO: Information_ratio")
}

modigliani_ratio <- function(log_returns_portfolio, log_returns_alternative, risk_free_return) {
  stop("TODO: modigliani_ratio")
}