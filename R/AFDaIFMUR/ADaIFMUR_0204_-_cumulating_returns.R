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

amzn_df <- amzn_df %>% dplyr::select(date, adjusted) %>%
  tq_mutate(select = adjusted,
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "daily_return") %>%
  dplyr::mutate(gross_return = 1. + daily_return,
                cum_gross_return = cumprod(gross_return),
                cum_net_return = cum_gross_return - 1) %>%
  dplyr::mutate(log_return = log(adjusted) - log(lag(adjusted)),
                cum_log_return = cumsum(log_return),
                cum_ret = exp(cum_log_return) - 1)

# plotting cumulative net returns and cumulative log returns
# shows virtually no difference, highlighting that the results are similar
# but computation with log returns is much simpler.

amzn_df %>%
  ggplot() +
  geom_line(aes(x = date, y = cum_net_return, colour = "R_d")) +
  geom_line(aes(x = date, y = cum_ret, colour = "L_d")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "date", y = "adjusted daily return (%)",
       title = "AMZN",
       subtitle = "Cumulative gross return") +
  theme_tq()