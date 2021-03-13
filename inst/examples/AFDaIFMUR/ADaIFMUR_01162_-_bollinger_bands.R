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

# The Bollinger Bands have three components. The first component is a 20-day simple
# moving average (SMA). The second component is an upper band,
# which is two standard deviations above the 20-day SMA.
# The third component is a lower band, which is two standard deviations
# below the 20-day SMA.

k <- 20;

amzn_close <- amzn_df %>%
  dplyr::select(date, close) %>%
  tq_mutate(
    # tq_mutate args
    select     = close,
    mutate_fun = rollapply,
    # rollapply args
    width      = k,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "ma_cl"
  ) %>%
  tq_mutate(
    # tq_mutate args
    select     = close,
    mutate_fun = rollapply,
    # rollapply args
    width      = k,
    align      = "right",
    FUN        = sd,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "sd_cl"
  );

# now let's plot - the data and the bollinger bands
amzn_close %>%
  # restrict to subset where all data are available
  dplyr::filter(date >= "2012-01-01") %>%
  dplyr::mutate(upper = ma_cl + 2 * sd_cl,
                lower = ma_cl - 2 * sd_cl) %>%
  ggplot() +
  geom_line(aes(x = date, y = close, colour = "close")) +
  geom_line(aes(x = date, y = ma_cl, colour = "ma_close")) +
  geom_line(aes(x = date, y = upper), colour = "blue") +
  geom_line(aes(x = date, y = lower), colour = "blue") +
  labs(title = "Amazon", y = "Closing ($)", x = "Date",
       sutitle = "Moving Average Indicator - Window length 1, 50, 200") +
  scale_x_date() +
  theme_tq();

# When the bands narrow, it may be used as an indication that volatility is
# about to rise. For a trend follower, when Amazon’s stock price was right
# around the upper band as in July 2013 and November 2013,
# this may be taken as an indication that the stock is overbought.
# Conversely, when Amazon.com’s stock price moved right around the lower band,
# as in August 2013, this may be taken as an indication that the stock is oversold.