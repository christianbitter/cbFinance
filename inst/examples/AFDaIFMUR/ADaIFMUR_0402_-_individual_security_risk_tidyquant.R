# auth: christian bitter
# desc: compute the risk and expected return for AMZN
# this time we use tidy quant and the tq_performance call

rm(list = ls());

library(dplyr);
library(ggplot2);
library(lubridate);
library(tidyquant);

source("inst/examples/AFDaIFMUR/common.R")

# aggregate to yearly returns

amzn_df <- get_asset(from = "2010-12-31", to = "2013-12-31") %>%
  adjusted.close() %>%
  dplyr::mutate(year = lubridate::year(date)) # %>%
  # dplyr::filter(year >= 2011);

# which performance functions are available: tq_performance_fun_options()
# standard: "mean", "sd" - applied to full data set
# table.AnnualizedReturns
# and annualized: "Return.annualized", "Return.annualized.excess", "sd.annualized", "SharpeRatio.annualized"

# note: annualized returns are not the same as the annualized mean returns
# https://www.investopedia.com/terms/a/annualized-total-return.asp
#
# An annualized total return is the geometric average amount of money earned by
# an investment each year over a given time period.
#
# The annualized return formula shows what an investor would earn over a period
# of time if the annual return was compounded.

# Calculating annualized rate of return needs only two variables:
# the returns for a given period and the time the investment was held.

amzn_yearly_returns_df <-
  amzn_df %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rd") %>%
  dplyr::left_join(amzn_df) %>%
  dplyr::group_by(year) %>%
  tq_performance(Ra = Rd, Rb = NULL, performance_fun = table.AnnualizedReturns)
# in order to have the "right" value for 2011 we need to leave the last day of
# 2010 in, that will produce the right value for sd and for some reason
# the negative value for annualized return

# compare annualized from other example
# A tibble: 4 x 4
# scope         mu  sigma    sd
# <chr>      <dbl>  <dbl> <dbl>
# 1 2011      0.0349 0.147  0.384
# 2 2012      0.423  0.101  0.318
# 3 2013      0.488  0.0732 0.270
# 4 2010-2013 0.315  0.107  0.327