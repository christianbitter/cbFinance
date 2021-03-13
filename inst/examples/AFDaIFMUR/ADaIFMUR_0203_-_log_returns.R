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
            col_rename = "daily_return") %>%
  dplyr::mutate(log_return = log(adjusted) - log(lag(adjusted)));

ibm_df %>%
  ggplot() +
  geom_line(aes(x = date, y = daily_return, colour = "R_d")) +
  geom_line(aes(x = date, y = log_return, colour = "R_l")) +
  labs(x = "date", y = "adjusted daily return (%)",
       title = "IBM", subtitle = "Comparison of daily log and standard returns") +
  theme_tq()