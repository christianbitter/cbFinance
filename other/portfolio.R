rm(list = ls());
library(tidyquant);
library(ggplot2);
library(dplyr);
library(lubridate);

symbols <- c("MSFT");

data <- tidyquant::tq_get(x = symbols, from = "2019-12-31", to = today());
data <- data %>% dplyr::mutate(date = lubridate::date(date));

#### prices
data %>%
  ggplot(aes(x = date, y = close,
             open = open, high = high, low = low, close = close,
             group = symbol)) +
  geom_barchart() +
  geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) +
  labs(title = "Portfolio Bar Chart",
       subtitle = "BBands with SMA Applied, Experimenting with Multiple Stocks",
       y = "Closing Price", x = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq()


#### volumes
data %>%
  ggplot(aes(x = date, y = volume)) +
  geom_segment(aes(xend = date, yend = 0, color = volume)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "AMZN Bar Chart",
       subtitle = "Charting Daily Volume, Zooming In",
       y = "Volume", x = "") +
  scale_color_gradient(low = "red", high = "darkblue") +
  theme_tq() +
  theme(legend.position = "none")