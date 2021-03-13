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

ibm_df <- get_asset(asset="IBM");

ibm_df <- ibm_df %>% dplyr::select(date, adjusted) %>%
  tq_mutate(select = adjusted,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "daily_return");

ibm_df %>%
  ggplot(aes(x = date, y = daily_return)) +
  geom_line() +
  labs(x = "date", y = "adjusted daily return (%)",
       title = "IBM", subtitle = "Daily return") +
  theme_tq()