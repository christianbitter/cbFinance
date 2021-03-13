# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc: computing monthly returns

#https://www.investopedia.com/terms/e/ema.asp

rm(list = ls());
library(tidyquant);
library(tidymodels);
library(ggplot2);
library(tidyr);
library(lubridate);

source("./R/AFDaIFMUR/common.R");

# in order to get the last day of the month use lubridate ceiling date
eom <- function(date)lubridate::ceiling_date(lubridate::date(date), "month") - 1

amzn_df <- get_asset("AMZN", to="2014-01-01");
ibm_df <- get_asset("IBM", to="2014-01-01");
msft_df <- get_asset("MSFT", to="2014-01-01");
sp500_df <- get_asset("^GSPC", alt_ticker = "SP500", to="2014-01-01");

# compute the returns over the time frame
amzn_df <- amzn_df %>% adjusted.close(rename = "amzn_adjusted");
ibm_df <- ibm_df %>% adjusted.close(rename = "ibm_adjusted");
msft_df <- msft_df %>% adjusted.close(rename = "msft_adjusted");
sp500_df <- sp500_df %>% adjusted.close(rename = "sp500_adjusted");

all_df <- amzn_df %>%
  dplyr::inner_join(ibm_df) %>%
  dplyr::inner_join(msft_df) %>%
  dplyr::inner_join(sp500_df) %>%
  dplyr::mutate(i = row_number()) %>%
  dplyr::filter(i == 1 | i == n());

# TODO: how can we use purrr
all_df <- all_df %>%
  dplyr::mutate(amzn = (amzn_adjusted - lag(amzn_adjusted, default=0)) / lag(amzn_adjusted, default=1),
                ibm = (ibm_adjusted - lag(ibm_adjusted, default=0)) / lag(ibm_adjusted, default=1),
                msft = (msft_adjusted - lag(msft_adjusted, default=0)) / lag(msft_adjusted, default=1),
                sp500 = (sp500_adjusted - lag(sp500_adjusted, default=0)) / lag(sp500_adjusted, default=1)) %>%
  dplyr::filter(i != 1) %>% dplyr::select(-1);

# constructing portfolio weights
# assume we want to invest the following
invest <- tibble(security = c("amzn", "ibm", "msft", "sp500"),
                 vol = c(50000, 10000, 30000, 10000));

invest <- invest %>% dplyr::mutate(pct = vol / sum(vol));

# now our portfolio return is simply the volume weighted return
m_sec <- all_df %>% dplyr::select(amzn, ibm, msft, sp500) %>% unlist();
m_sec <- matrix(m_sec);
m_invest <- matrix(invest$pct);

port.ret <- t(m_invest) %*% m_sec;
