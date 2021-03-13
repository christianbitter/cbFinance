# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc: computing price returns

#https://www.investopedia.com/terms/e/ema.asp

rm(list = ls());
library(tidyquant);
library(tidymodels);
library(ggplot2);
library(skimr);

source("./R/AFDaIFMUR/common.R");

amzn_df <- get_asset(asset="AMZN");

amzn_df <- amzn_df %>%
  dplyr::select(date, close, adjusted) %>%
  tq_mutate(select = adjusted,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "Rt") %>%
  tq_mutate(select = close,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "Rp");

# now how much difference does it make between price and total return

amzn_df %>%
  ggplot() +
  geom_line(aes(x = date, y = Rp, colour = "price")) +
  geom_line(aes(x = date, y = Rt, colour = "total")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "date", y = "adjusted daily return (%)",
       title = "AMZN",
       subtitle = "Comparison of returns") +
  theme_tq()