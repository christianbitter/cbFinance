rm(list = ls());

library(tidyquant);
library(ggplot2);
library(dplyr);
library(cbFinance);
library(purrr);

# get the s/p500 and some stocks
assets <- tibble("symbol" = c("^GSPC", "IBM", "MSFT", "AAPL"),
                 "type" = c("etf", "stock", "stock", "stock"),
                 "name" = c("SP500", "IBM", "Microsoft", "Apple"));

data <- tq_get(x = assets$symbol, from = "2015-12-31", to = "2021-01-01");
data <- data %>% dplyr::inner_join(assets, by = "symbol");

daily_returns <- data %>%
  dplyr::select(adjusted, name, date) %>%
  dplyr::group_by(name) %>%
  tq_mutate(select = adjusted,
            mutate_fun = periodReturn,
            col_rename = "Rd",
            period = "daily",
            type = "log");

daily_returns %>%
  ggplot() +
  geom_line(aes(x = date, y = adjusted)) +
  labs(title = "Asset comparison across time",
       y = "Adjusted closing price ($)",
       x = "Date") +
  facet_grid(name ~ ., scales = "free") +
  theme_tq();

# again, we see that apple and microsoft track the sp500 quite strongly
# however, ibm is somewhat different

# compute the volatility, semideviation, and expected short fall
funs <- c(VaR = VaR, ES = ES);
args <- list(na.rm = TRUE)

# TODO: make this nicer using purrr
risk_summary <- daily_returns %>%
  dplyr::group_by(name) %>%
  tidyquant::tq_performance(
    Ra = Rd,
    performance_fun = VaR
  ) %>% dplyr::inner_join(
    daily_returns %>% dplyr::group_by(name) %>%
      tidyquant::tq_performance(Ra = Rd, performance_fun = ES)
  ) %>% dplyr::inner_join(
    daily_returns %>% dplyr::group_by(name) %>%
      tidyquant::tq_performance(Ra = Rd,
                                performance_fun = SharpeRatio.annualized)
  ) %>% dplyr::inner_join(
    daily_returns %>% dplyr::group_by(name) %>%
      tidyquant::tq_performance(Ra = Rd,
                                performance_fun = sd.annualized)
  ) %>% dplyr::inner_join(
    daily_returns %>% dplyr::group_by(name) %>%
      tidyquant::tq_performance(Ra = Rd,
                                performance_fun = SemiDeviation)
  ) %>% dplyr::inner_join(
    daily_returns %>% dplyr::group_by(name) %>%
      tidyquant::tq_performance(Ra = Rd,
                                performance_fun = AverageDrawdown)
  ) %>% dplyr::rename(Sharpe.Annualized = `AnnualizedSharpeRatio(Rf=0%)`,
                      SemiDeviation = `Semi-Deviation`,
                      Volatility.Annualized = "AnnualizedStandardDeviation")

# other risk measures we might look at are sortino ratio, which uses the semideviation, instead of volatility
# or the conditional sharpe, which uses expected shortfall as the unit of risk scaling


risk_summary %>%
  dplyr::inner_join(assets, by = "name") %>%
  tidyr::pivot_longer(cols = c("VaR", "ES", "Sharpe.Annualized", "Volatility.Annualized", "SemiDeviation",
                               "AverageDrawdown"),
                      names_to = "measure",
                      values_to = "value") %>%
  ggplot() +
  geom_col(aes(x = reorder(name, -value), y = value, fill = measure), position = "dodge") +
  scale_y_continuous() +
  labs(x = "Calendar Year", y = "Risk Measure", fill = "Risk Measure",
       title = "Asset Risk",
       subtitle = "Comparison of assets by different risk measures") +
  facet_grid(. ~ measure, scales = "free");

# here, we see that not a single risk measure captures the behaviour of all of
# our assets. On many of the risk measures the behaviour of individual assets is somewhat
# similar, here we should show their differences to the SP500 maybe.

# specifically, Microsoft vs. IBM needs some further exposition here
# because IBM seems to fare much better in terms of VaR, annualized volatility and
# semideviation. however, looking at average drawdown, i.e. the average of the max drawdown
# we can see that ibm is worse than microsoft. Also, if we look at annualized sharpe ratio
# i.e. excess return over a risk free asset in units of annualized volatility
# we can see that (1) IBM has negative sharpe/A, meaning that IBM fared worse
# than a risk free asset in some instances. Furthermore, microsoft exceeded the
# SP500 by a large margin as well. And while it fared worse than APPL in Sharpe/A
# the other risk measures display lower risk when compared to APPL.
# so overall Microsoft is a good investment it seems.