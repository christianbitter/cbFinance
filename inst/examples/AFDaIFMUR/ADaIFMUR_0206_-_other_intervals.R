# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc: computing monthly returns

#https://www.investopedia.com/terms/e/ema.asp

rm(list = ls());
library(tidyquant);
library(tidymodels);
library(ggplot2);
library(skimr);
library(lubridate);

source("./R/AFDaIFMUR/common.R");

# in order to get the last day of the month use lubridate ceiling date
eom <- function(date)lubridate::ceiling_date(lubridate::date(date), "month") - 1

amzn_df <- get_asset(asset="AMZN");

amzn_df <- amzn_df %>%
  dplyr::mutate(is_eom = date == eom(date)) %>%
  dplyr::filter(is_eom == T) %>%
  dplyr::select(date, close) %>%
  dplyr::mutate(Rm = close - lag(close))

# now compute the log returns
amzn_df <- amzn_df %>%
  dplyr::mutate(rt = log(close) - log(lag(close)),
                Rp = exp(rt) - 1.);
# get rid of the first observation
amzn_df <- amzn_df[-1, ];

amzn_df %>%
  ggplot() +
  geom_line(aes(x = date, y = Rp)) +
  geom_smooth(aes(x = date, y = Rp), method = "lm") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "date", y = "Monthly returns (%)",
       title = "AMZN",
       subtitle = "Amazon monthly returns") +
  theme_tq();