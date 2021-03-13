# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc: getting the AMZN data

rm(list = ls());
library(tidyquant);
library(tidymodels);
library(ggplot2);
library(skimr);

source("./R/AFDaIFMUR/common.R");

amzn_df <- get_asset();

# as a first technical indicator, we are going to look into
# a trend indicator. Specifically, a 50 and 200 day moving average.

# for a reference on potential implementation look here:
# https://www.business-science.io/timeseries-analysis/2017/07/23/tidy-timeseries-analysis-pt-2.html
# The tq_mutate() function always adds columns to the existing data frame

# since 200 is multiply 50 we can smooth over the output of the former
amzn_close <- amzn_df %>%
  dplyr::select(date, close) %>%
  tq_mutate(
    # tq_mutate args
    select     = close,
    mutate_fun = rollapply,
    # rollapply args
    width      = 50,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "mean_50"
  ) %>%
  tq_mutate(
    # tq_mutate args
    select     = close,
    mutate_fun = rollapply,
    # rollapply args
    width      = 200,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "mean_200"
  );

# now let's plot
amzn_close %>%
  # restrict to subset where all data are available
  dplyr::filter(date >= "2012-01-01") %>%
  ggplot() +
  geom_line(aes(x = date, y = close, colour = "close")) +
  geom_line(aes(x = date, y = mean_50, colour = "mean_50")) +
  geom_line(aes(x = date, y = mean_200, colour = "mean_200")) +
  labs(title = "Amazon", y = "Closing ($)", x = "Date",
       sutitle = "Moving Average Indicator - Window length 1, 50, 200") +
  scale_x_date() +
  theme_tq();

# If the 50-day moving average cross above the 200-day moving average,
# which is called a bullish crossover, this may be taken as an indicator
# to buy the stock. Conversely, if the 50-day moving average
# crosses below the 200-day moving average, which is known as a bearish crossover,
# this may be taken as an indication to sell the stock.