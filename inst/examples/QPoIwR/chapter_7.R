# name: chrapter_7.r
# desc: exercises from chapter 7 of - A Quantitative Primer on Investments with R
# auth: (c) christian bitter - 2021

rm(list = ls());
library(ggplot2);
library(moments)
library(tidyquant);
library(tidyr);
library(dplyr);
library(ggcorrplot);

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


# 7.7.1 Risk free price risk
# download US constant treasuries with different maturity dates
library(Quandl);
library(dplyr);

codes <- c("FRED/DGS3MO", "FRED/DGS2", "FRED/DGS10", "FRED/DGS30");
ust.raw <- Quandl(codes, type = "xts");
colnames(ust.raw) <- c("T3M.yld", "T2Y.yld", "T10Y.yld", "T30Y.yld");

# the ylds are in percentage points, so we need to divide them
ust.raw <- ust.raw / 100.;

autoplot.zoo(ust.raw) +
  theme_light() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "date", y = "Yld");


# a) compute an average yield for each of the constant treasuries
average.yld <- ust.raw %>%
  fortify() %>%
  dplyr::summarise(avg_T3M = mean(T3M.yld, na.rm = T),
                   avg_T2Y = mean(T2Y.yld, na.rm = T),
                   avg_T10Y = mean(T10Y.yld, na.rm = T),
                   avg_T30Y = mean(T30Y.yld, na.rm = T));

# so we see the best average yield is for the 30Y treasury, which
# yields about 6.4%.

# b) calculate daily log returns via an approximation
# since these are percentage values, we cannot compute daily log returns
# directly. First compute the change in yields

ust.raw$dT3M <- diff(ust.raw$T3M.yld);
ust.raw$dT2Y <- diff(ust.raw$T2Y.yld);
ust.raw$dT10Y <- diff(ust.raw$T10Y.yld);
ust.raw$dT30Y <- diff(ust.raw$T30Y.yld);

# now the differences need to be multiplied with the average time of a bond's
# cashflow. This results in an approximation of a daily difference in bond price
# which is on the same scale as the percentage yields
ust.raw$dT3M <- ust.raw$dT3M * -.25;
ust.raw$dT2Y <- ust.raw$dT2Y * -1.98;
ust.raw$dT10Y <- ust.raw$dT10Y * -8.72;
ust.raw$dT30Y <- ust.raw$dT30Y * -19.2;
# from this we can compute expected/ average daily log returns
# which form the basis for annualized average return or profit
ust.raw_avg_dT3M <- mean(ust.raw$dT3M, na.rm = T);
ust.raw_avg_dT2Y <- mean(ust.raw$dT2Y, na.rm = T);
ust.raw_avg_dT10Y <- mean(ust.raw$dT10Y, na.rm = T);
ust.raw_avg_dT30Y <- mean(ust.raw$dT30Y, na.rm = T);

trading_days <- 250;
ust.raw_avg_aT3M <- ust.raw_avg_dT3M * trading_days;
ust.raw_avg_aT2Y <- ust.raw_avg_dT2Y * trading_days;
ust.raw_avg_aT10Y <- ust.raw_avg_dT10Y * trading_days;
ust.raw_avg_aT30Y <- ust.raw_avg_dT30Y * trading_days;

# the 30Y treasury has an annualized expected profit of 2.5%,
# which is the highest

#3. compute daily volatility
ust.raw_vol_dT3M <- sd(ust.raw$dT3M, na.rm = T);
ust.raw_vol_dT2Y <- sd(ust.raw$dT2Y, na.rm = T);
ust.raw_vol_dT10Y <- sd(ust.raw$dT10Y, na.rm = T);
ust.raw_vol_dT30Y <- sd(ust.raw$dT30Y, na.rm = T);
# and annualize it
ust.raw_vol_aT3M <- ust.raw$vol_dT3M * sqrt(trading_days);
ust.raw_vol_aT2Y <- ust.raw$vol_dT2Y * sqrt(trading_days);
ust.raw_vol_aT10Y <- ust.raw$vol_dT10Y * sqrt(trading_days);
ust.raw_vol_aT30Y <- ust.raw$vol_dT30Y * sqrt(trading_days);
# while the 30Y treasury has the highest profit,
# it also has the highest annualized volatility

# 4. calculate kurtosis and skewness
k_T3M <- moments::kurtosis(x = ust.raw$dT3M, na.rm = T);
s_T3M <- moments::skewness(x = ust.raw$dT3M, na.rm = T);

# look at the histogram of approximated daily log returns
ust.raw %>%
  fortify() %>%
  dplyr::select(dT3M, dT2Y, dT10Y, dT30Y) %>%
  tidyr::pivot_longer(cols = c(dT3M, dT2Y, dT10Y, dT30Y),
                      names_to = "Treasury", values_to = "DailyLogPrice",
                      values_drop_na = T) %>%
  ggplot() +
  geom_histogram(aes(x = DailyLogPrice)) +
  scale_x_continuous(labels = scales::percent) +
  theme_light() +
  labs(x = "Daily Log Price Percentage Change",
       title = "US Treasuries",
       caption = "(c) 2021 Christian Bitter - Data Quandl") +
  facet_grid(. ~ Treasury);

# we can see that the 30Y treasury is much wider, and while still concentrated
# around 0%, there is actually some daily price change
# as opposed to the 3 month treasury, which on first glance is
# well concentrated around 0%

# 7.3 - Equity Price Risk
equities <- tibble(
  "symbol" = c("^GSPC", #S&P500
               "MSFT", #Microsoft
               "AAPL" #Apple
  ),
  "name" = c("SP500", "Microsoft", "Apple")
);

data <- tidyquant::tq_get(x = equities$symbol, from = "2010-01-01");
equities <- data %>% dplyr::inner_join(equities, by = "symbol");

equities %>%
  ggplot(aes(x = date)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = adjusted)) +
  labs(title = "Equities - Daily Price", y = "Closing Price", x = "") +
  theme_tq() +
  facet_grid(name ~ ., scales = "free")

# if we look at AAPL, MSFT and S&P500 they track each other quite closely
# i) average price
mean_price <- equity_return %>%
  dplyr::summarise(mean_price_SP500 = mean(SP500, na.rm = T),
                   mean_price_MSFT = mean(Microsoft, na.rm = T),
                   mean_price_AAPL = mean(Apple, na.rm = T));

equities %>%
  ggplot(aes(x = date)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = adjusted)) +
  labs(title = "Equities - Daily Price", y = "Closing Price", x = "") +
  theme_tq() +
  facet_grid(name ~ ., scales = "free")

# ii) compute log returns
equity_return <-
  equities %>% dplyr::select(name, date, adjusted) %>%
  tidyr::pivot_wider(id_cols = c("name", "date"), names_from = "name", values_from = adjusted);

returns <- equity_return %>%
  dplyr::mutate(SP500_lPrice = log(SP500),
                MSFT_lPrice = log(Microsoft),
                AAPL_lPrice = log(Apple)) %>%
  dplyr::summarise(SP500_ldRet = c(0, diff(SP500_lPrice)),
                   MSFT_ldRet  = c(0, diff(MSFT_lPrice)),
                   AAPL_ldRet  = c(0, diff(AAPL_lPrice))) %>%
  dplyr::bind_cols(equity_return);

returns %>%
  tidyr::pivot_longer(cols = c("SP500_ldRet", "MSFT_ldRet", "AAPL_ldRet"),
                      names_to = "name", values_to = "adjusted") %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  labs(title = "Equities - Daily Log Returns", y = "log(R_d)", x = "Date") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date() +
  theme_tq() +
  facet_grid(name ~ ., scales = "free");

# average log return and annualized volatility
trading_days <- 250;
returns %>%
  tidyr::pivot_longer(cols = c("SP500_ldRet", "MSFT_ldRet", "AAPL_ldRet"),
                      names_to = "name", values_to = "adjusted") %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(mean_ldRet = mean(adjusted, na.rm = T),
                   annualized_volatility = sqrt(trading_days) * sd(adjusted, na.rm = T),
                   skew = moments::skewness(adjusted),
                   kurt = moments::kurtosis(adjusted));

# let's look at the histogram of daily log returns to understand the skewness better
returns %>%
  tidyr::pivot_longer(cols = c("SP500_ldRet", "MSFT_ldRet", "AAPL_ldRet"),
                      names_to = "name", values_to = "adjusted") %>%
  dplyr::select(name, adjusted) %>%
  ggplot() +
  geom_histogram(aes(x = adjusted), binwidth = .001) +
  labs(x = "Daily Log Return", y = "#", title = "Equities - Daily Log Returns") +
  theme_tq() +
  scale_x_continuous(labels = scales::percent) +
  facet_grid(name ~ ., scales = "free");
# skewness as a measure for symmetry indicates how symmetric our distributions around the center point.
# the (symmetric) normal distribution has skewness of 0
# our data for the 3 assets indicates some left-skewness - the left tail is long relative to the right tail
# meaning that we may exhibit larger losses, i.e. a wider range of negative daily log returns

# positive kurtosis indicates heavy tails (the normal distribution has kurtosis of 3),
# and we see that the distribution of daily log returns across all assets
# are spread out and less concentrated around the mean.
# However, we can see that AAPL has a fatter belly, i.e. is more strongly concentrated around the mean
# of zero. whereas MSFT and SP500 are less concentrated and spread out

# overall our distributions resemble more the double exponential or Gauchy in their
# belly and tails behavior

# correlated between the equities?
# relatively low annualized volatility
equities %>%
  dplyr::select(name, date, adjusted) %>%
  tidyr::pivot_wider(id_cols = c("name", "date"), names_from = "name", values_from = adjusted) %>%
  dplyr::select(-date) %>%
  pairs()

# here we already see some linear association between the equities
corr <- equities %>%
  dplyr::select(name, date, adjusted) %>%
  tidyr::pivot_wider(id_cols = c("name", "date"), names_from = "name", values_from = adjusted) %>%
  dplyr::select(-date) %>% cor()

# the correlation matrix makes this just more obvious
corr %>% ggcorrplot() +
  labs(title = "Correlation between equities") +
  theme_light()