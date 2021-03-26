# auth: christian bitter
# desc: risk of two assets, manual approach

rm(list = ls());

library(dplyr);
library(ggplot2);
library(lubridate);
library(tidyquant);

source("inst/examples/AFDaIFMUR/common.R");

# In the case of a two-asset portfolio
# the risk - here variance can be calculated as
# sigma2_p = w1^2 * sigma2_s1 + w2^2 * sigma2_s2^2 + cov(s1, s2) * w1 * w2

# assume a portfolio of 25% amzn and 75% ibm, convert to matrix
weights <- c("AMZN" = .25, "IBM" = .5, "TSLA" = .25);
weights <- matrix(weights, nrow = 1,
                  dimnames = list(c("weight"), asset = c("AMZN", "IBM", "TSLA")));
# weights <- t(weights);

amzn <- get_asset() %>% adjusted.daily.returns(name.return = "AMZN");
ibm  <- get_asset(asset = "IBM") %>% adjusted.daily.returns(name.return = "IBM");
tsla <- get_asset(asset = "TSLA") %>% adjusted.daily.returns(name.return = "TSLA");

# combine and drop the first observation
portfolio <- amzn %>%
  dplyr::inner_join(ibm, by = "date") %>%
  dplyr::inner_join(tsla, by = "date") %>%
  dplyr::select(date, AMZN, IBM, TSLA);

portfolio <- portfolio[-1, ];

# compute the variance-covariance matrix
m_portfolio <- as.matrix(portfolio[, 2:4]);
m_cov_portfolio <- cov(m_portfolio);

# compute the annualized variance-covariance matrix
trading_days <- 252;
n_cov_portfolio <- m_cov_portfolio * trading_days;

# now we can use matrix algebra to get the portfolio risk
portfolio_risk <- weights %*% n_cov_portfolio %*% t(weights);

# we can see that using matrix algebra makes adding another asset
# to the portfolio rather easy and straight forward
# we can also see that through adding tesla and rebalancing the portfolio
# we carry now more risk. if we look at the var-cov matrix
print(m_cov_portfolio)
# we can recognize that IBM has the lowest risk and tsla has a higher risk than
# amazon. consequently, adding tesla not only reduced the 'stability' of ibm
# but also added more risk.

# let's see what the weighed standard deviation as a crude approximation for risk
# would be
w_sd <- n_cov_portfolio[1, 1] * weights[1] +
  n_cov_portfolio[2, 2] * weights[2] +
  n_cov_portfolio[3, 1] * weights[3];

# 0.0570075 as opposed to 0.05472667