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
weights <- c("AMZN" = .25, "IBM" = .75);
weights <- matrix(weights, nrow = 1,
                  dimnames = list(c("weight"), asset = c("AMZN", "IBM")));
# weights <- t(weights);

amzn <- get_asset() %>% adjusted.daily.returns(name.return = "AMZN");
ibm  <- get_asset(asset = "IBM") %>% adjusted.daily.returns(name.return = "IBM");

# combine and drop the first observation
portfolio <- amzn %>% dplyr::inner_join(ibm, by = "date") %>% dplyr::select(date, AMZN, IBM);
portfolio <- portfolio[-1, ];

# compute the variance-covariance matrix
m_portfolio <- as.matrix(portfolio[, 2:3]);
m_cov_portfolio <- cov(m_portfolio);

# compute the annualized variance-covariance matrix
trading_days <- 252;
n_cov_portfolio <- m_cov_portfolio * trading_days;

# now we can use matrix algebra to get the portfolio risk
portfolio_risk <- weights %*% n_cov_portfolio %*% t(weights);

# this matches perfectly our manual risk calculation: risk = 0.03626203