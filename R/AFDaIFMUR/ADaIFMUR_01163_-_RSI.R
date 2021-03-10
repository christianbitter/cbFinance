# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc: getting the AMZN data

#https://www.investopedia.com/terms/e/ema.asp

rm(list = ls());
library(tidyquant);
library(tidymodels);
library(ggplot2);
library(skimr);

source("./R/AFDaIFMUR/common.R");

amzn_df <- get_asset();

# A common technical analysis momentum indicator is the Relative Strength Index
# (RSI).
# RSI = 100 - (100 / 1 + RS)
# RS is equal to the up average divided by the down average with the averages
# calculated using the Wilder Exponential Moving Average described below

# compute the delta
amzn_rsi_df <- amzn_df %>% dplyr::select(date, close);
amzn_rsi_df$close_delta <- c(NA_real_, diff(amzn_rsi_df$close));
amzn_rsi_df <- amzn_rsi_df %>%
  dplyr::mutate(up = close_delta > 0,
                down = close_delta < 0,
                up_val = up * close_delta,
                # note that we report absolute values
                down_val = -1. * down * close_delta);

window_width <- 14;

amzn_rsi_df <- amzn_rsi_df %>%
  tq_mutate(
    # tq_mutate args
    select     = up_val,
    mutate_fun = EMA,
    # rollapply args
    n      = window_width,
    wilder = T,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "up_avg"
  ) %>%
  tq_mutate(
    # tq_mutate args
    select     = down_val,
    mutate_fun = EMA,
    # rollapply args
    n      = window_width,
    wilder = T,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "down_avg"
  ) %>%
  dplyr::mutate(RS = up_avg / down_avg) %>%
  dplyr::mutate(RSI = 100 - (100 / (1 + RS)));

# Key Takeaways
#
# The EMA is a moving average that places a greater weight and significance on the most recent data points.
# Like all moving averages, this technical indicator is used to produce buy and sell signals based on crossovers and divergences from the historical average.
# Traders often use several different EMA lengths, such as 10-day, 50-day, and 200-day moving averages.

amzn_rsi_df %>%
  dplyr::filter(date >= "2012-01-01") %>%
  dplyr::filter(date <= "2013-01-01") %>%
  ggplot() +
  geom_line(aes(x = date, y = RSI)) +
  geom_hline(aes(x = date, yintercept = 30, colour = "lower")) +
  geom_hline(aes(x = date, yintercept = 70, colour = "upper")) +
  theme_light()

#
# The RSI is used in conjunction with an overbought line and an oversold line.
# The overbought line is typically set at a level of 70 and the oversold
# line is typically set at a level of 30.
# A buy signal is created when the RSI rises from below the oversold
# line and crosses the oversold line. Conversely, a sell signal is created when
# the RSI falls from above the overbought line and crosses the overbought line.

# TODO: generate the signals
amzn_rsi_df <- amzn_rsi_df %>%
  dplyr::mutate(buy_signal = lag(RSI) <= 30 & RSI >= 30,
                sell_signal = lag(RSI) >= 70 & RSI <= 70)

amzn_rsi_df %>%
  dplyr::filter(date >= "2012-01-01") %>%
  dplyr::filter(date <= "2013-01-01") %>%
  ggplot() +
  geom_line(aes(x = date, y = RSI)) +
  geom_hline(aes(x = date, yintercept = 30, colour = "lower")) +
  geom_hline(aes(x = date, yintercept = 70, colour = "upper")) +
  geom_vline(data = amzn_rsi_df[amzn_rsi_df$sell_signal == T, ],
             aes(xintercept = date, colour = "sell")) +
  geom_vline(data = amzn_rsi_df[amzn_rsi_df$buy_signal == T, ],
             aes(xintercept = date, colour = "buy")) +
  labs(x = "date", y = "Relative Strength Index",
       title = "Amazon Momentum", colour = "signal",
       subtitle = "RSI-based buy and sell signals.") +
  theme_tq()