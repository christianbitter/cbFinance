rm(list = ls());

library(tidyquant);
library(ggplot2);
library(dplyr);
library(lubridate);

assets <- tibble(
  "symbol" = c("MSFT", #Microsoft
               "CS.PA", #AXA SA"
               "BAS.DE", #BASF
               "BAYN.DE", #Bayer
               "BP", #BP
               "FB", #FB
               "IS3R.DE",  #ISIV-E.MSCI WMF U.ETF DLA
               "SPPW.DE"
                 ),
  "name" = c("Microsoft Corporation",
             "AXA S.A. INH.",
             "BASF SE NA O.N.",
             "BAYER AG NAMENS-AKTIEN O.N.",
             "BP PLC",
             "FACEBOOK INC.",
             "ISIV-E.MSCI WMF U.ETF DLA",
             "SPDR MSCI World UCITS ETF"
             )
);

data <- tidyquant::tq_get(x = assets, from = "2015-12-31", to = today());
data <- data %>%
  dplyr::mutate(date = lubridate::date(date)) %>%
  dplyr::inner_join(assets);

# loaded symbols
data %>% dplyr::count(symbol)

#### prices
data %>%
  ggplot(aes(x = date, y = close,
             open = open, high = high, low = low, close = adjusted,
             group = name)) +
  geom_barchart() +
  geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) +
  labs(title = "Portfolio",
       subtitle = "Daily Asset Performance",
       y = "Adjusted Closing Price", x = "") +
  facet_wrap(~ name, ncol = 2, scales = "free_y") +
  theme_tq()