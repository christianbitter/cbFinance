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

# assume a portfolio of 25% amzn and 75% ibm
weights <- tibble("symbol" = c("AMZN", "IBM"),
                  "w" = c(.25, .75));

amzn <- get_asset() %>% adjusted.daily.returns(name.return = "AMZN");
ibm  <- get_asset(asset = "IBM") %>% adjusted.daily.returns(name.return = "IBM");

# combine and drop the first observation
portfolio <- amzn %>% dplyr::inner_join(ibm, by = "date") %>% dplyr::select(date, AMZN, IBM);
portfolio <- portfolio[-1, ];

# compute the annualized variance for each security and
# compute the annualized covariance for all asset combinations
trading_days <- 252;
f <- sqrt(trading_days);

portfolio_risk <- portfolio %>%
  dplyr::summarise(sd_AMZN_a = sd(AMZN) * f,
                   sd_IBM_a = sd(IBM) * f,
                   cov_a = cov(AMZN, IBM) * trading_days);

# now calculate the portfolio risk
w_AMZN <- weights %>% dplyr::filter(symbol == "AMZN") %>% .[["w"]];
w_IBM  <- weights %>% dplyr::filter(symbol == "IBM") %>% .[["w"]];
# var(X + Y) = var(X) + var(Y) + 2 COV(X, Y)
risk <- portfolio_risk$sd_AMZN_a^2 * w_AMZN^2 + portfolio_risk$sd_IBM_a^2 * w_IBM^2 +
        2 * portfolio_risk$cov_a * w_AMZN * w_IBM;
risk_sd <- sqrt(risk);

# Note that this portfolio standard deviation is lower than the weighted-average
# standard deviation of AMZN and IBM returns.
# This is due to the covariance of not aligned assets showing the power of diversification.
# That is, through careful selection of non-aligned (covariance) assets, the overall
# portfolio has lower risk (risk profiles balance) than an equally weighed portfolio
# with correlation-aligned assets.
w_sd <- portfolio_risk$sd_AMZN_a * w_AMZN + portfolio_risk$sd_IBM_a * w_IBM;