rm(list = ls());
library(ggplot2);
library(moments)

# plot a random walk
random_walk(x0 = 100, t = 100) %>%
  ggplot() +
  geom_point(aes(x = t, y = x, colour = e)) +
  geom_line(aes(x = t, y = x), colour = "gray") +
  labs(x = "t", y = "Price", title = "Martingale/ Random Walk",
       colour = "Increment") +
  theme_light()

######
# quiz
######

# 1. given the following data
x <- c(2, 5, 1, 4, -1, 0, 5, -3, 1, 3, 2, 5);
# compute mean, variance, sd, kurtosis and skew

plot(x, type = 'l')
points(x)

m_x <- mean(x);
var_x <- var(x);
sd_x <- sqrt(var_x);
k_x <- moments::kurtosis(x);
s_x <- moments::skewness(x);

# 2.

# 6. if the daily volatility of a stocks log return is .03
# what is annualized volatility
sd_daily <- .03;
trading_days <- 250;
sd_annualized <- sd_daily * sqrt(trading_days);


# exercise
library(Quandl);
library(quantmod);
library(tidyquant);
library(ggplot2);

# download some stocks
stocks <- tidyquant::tq_get(x = c("IBM", "AAPL"), from = "2019-12-31", to = "2021-01-10");

ibm <- stocks %>% dplyr::filter(symbol == "IBM");
aapl <- stocks %>% dplyr::filter(symbol == "AAPL");


ibm %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "IBM Candlestick Chart", y = "Closing Price", x = "") +
  theme_tq();

aapl %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "AAPL Candlestick Chart", y = "Closing Price", x = "") +
  theme_tq();


# look at the histogram of daily prices
aapl %>%
  ggplot() +
  geom_histogram(aes(x = close)) +
  labs(title = "AAPL Daily Price Histogram", x = "Closing Price (USD", y = '#') +
  theme_tq();
# we see that this is strongly not normal - but rather bi-modal or something
L <- length(aapl$close);
log_p <- c(0, log(aapl$close[2:L]) - log(aapl$close[1:(L - 1)]));
# this is the same as diff(log(aapl$close))


aapl$log_return <- log_p;

aapl %>%
  ggplot() +
  geom_histogram(aes(x = log_return)) +
  labs(title = "AAPL Daily Log Returns", x = "Log Return", y = '#') +
  theme_tq();

# log return do look more well behaved
aapl %>%
  ggplot(aes(x = date, y = log_return)) +
  geom_point() + geom_line() +
  labs(title = "AAPL Daily Log Returns",
       subtitle = "Daily log returns resemble the error components in a random walk process",
       x = "Log Return", y = '#') +
  theme_tq();

# let's look at cumulative daily log returns
aapl %>%
  dplyr::summarize(cdaily_log_return = cumsum(log_return)) %>%
  dplyr::bind_cols(aapl) %>%
  ggplot(aes(x = date, y = cdaily_log_return)) +
  geom_point() + geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "AAPL Daily",
       subtitle = "AAPL Cumulative daily log returns. We can see that AAPL gained a massive 50% over the year.",
       x = "Log Return", y = '#') +
  theme_tq();

# so over the curse of the year the aapl stock gained 50%, also not the loss
# during april, here we went down about 25%
# so the max. gain is more like 75% if you happened to enter the stock at the right time

day_1 <- aapl$close[1];
day_n <- aapl$close[nrow(aapl)]

increase <- (day_n - day_1) / day_1

# so what is the annualized average daily return
trading_days <- 250;
mean(aapl$log_return) * trading_days # .566 - 56%
# what is the annualized daily volatility
sd(aapl$log_return) * sqrt(trading_days) #.46 - 46%
# so while we made huge ground, we can also recognize that we had to endure some volatility
