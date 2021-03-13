# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc: computing monthly returns

#https://www.investopedia.com/terms/e/ema.asp

rm(list = ls())

library(tidyquant)
library(tidymodels)
library(ggplot2);
library(tidyr)
library(lubridate)

source("./R/AFDaIFMUR/common.R")

amzn_df <- get_asset("AMZN") %>% adjusted.close("AMZN")
ibm_df <- get_asset("IBM") %>% adjusted.close("IBM")
msft_df <- get_asset("MSFT") %>% adjusted.close("MSFT")
sp500_df <- get_asset("^GSPC") %>% adjusted.close("SP500")

all_df <- amzn_df %>%
  dplyr::inner_join(ibm_df) %>%
  dplyr::inner_join(msft_df) %>%
  dplyr::inner_join(sp500_df);

returns_df <- all_df %>%
  tidyr::pivot_longer(cols = c("AMZN", "IBM", "MSFT", "SP500"),
                      names_to = "symbol", values_to = "adjusted") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "Ra") %>%
  dplyr::filter(date != "2010-12-31");

returns_df %>%
  ggplot() +
  geom_col(aes(x = symbol, y = Ra, fill = symbol)) +
  geom_text(aes(x = symbol, y = Ra, label = sprintf("%s %%", 100 * round(Ra, 2)))) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "date", y = "Return (%)",
       title = "Performance of securities", subtitle = "Yearly return comparison") +
  facet_wrap(. ~ date) +
  theme_tq();


# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ05-performance-analysis-with-tidyquant.html
# The tidyquant function, tq_portfolio() aggregates a group of individual assets
# into a single return using a weighted composition of the underlying assets.
invest <- tibble(symbols = c("AMZN", "IBM", "MSFT", "SP500"),
                 vol = c(50000, 10000, 30000, 10000));

invest <- invest %>%
  dplyr::mutate(weights = vol / sum(vol)) %>%
  dplyr::select(-vol);


portfolio_df <- all_df %>%
  tidyr::pivot_longer(cols = c("AMZN", "IBM", "MSFT", "SP500"),
                      names_to = "symbol", values_to = "adjusted") %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "yearly",
               col_rename = "Ra") %>%
  dplyr::filter(date != "2010-12-31");

portfolio_returns <- portfolio_df %>%
  tq_portfolio(assets_col  = symbol,
               returns_col = Ra,
               weights     = invest,
               col_rename  = "Ra");

portfolio_returns %>%
  ggplot() +
  geom_col(aes(x = date, y = Ra)) +
  geom_text(aes(x = date, y = Ra, label = sprintf("%s %%", 100 * round(Ra, 2)))) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "date", y = "Return (%)",
       title = "Portfolio Performance", subtitle = "Yearly return comparison") +
  theme_tq();