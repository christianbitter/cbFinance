library(dplyr);
library(tidyquant);
library(ggplot2);


symbol <- c("MSFT");

data <- tidyquant::tq_get(x = symbol, from = "2015-12-31", to = "2021-01-01");

data <- data %>%
  tidyquant::tq_mutate(select = adjusted,
                       mutate_fun = periodReturn,
                       period = "daily",
                       type = "log");

# now look at the value at risk for the alpha = 5%, we set the notional value of
# the portfolio to 1 USD
alpha <- .05;
notional_value <- 1.;
v_alpha_r <- data %>% dplyr::select(daily.returns) %>% .[[1]] %>% quantile(probs = alpha);
volatility <- data %>% dplyr::select(daily.returns) %>% .[[1]] %>% cbFinance::volatility();
semideviation <- data %>% dplyr::select(daily.returns) %>% .[[1]] %>% cbFinance::semideviation();
v_alpha_r <- v_alpha_r * notional_value;

data %>%
  ggplot(aes(x = daily.returns)) +
  geom_histogram(binwidth = .001) +
  geom_histogram(data = subset(data, daily.returns <= v_alpha_r),
                 binwidth = .001, fill = "red") +
  geom_vline(aes(xintercept = v_alpha_r), colour = "red") +
  geom_label(aes(x = v_alpha_r, y = 0, label = round(v_alpha_r, 3))) +

  geom_vline(aes(xintercept = semideviation), colour = "red") +
  geom_label(aes(x = semideviation, y = 0, label = round(semideviation, 3))) +

  geom_vline(aes(xintercept = volatility), colour = "blue") +
  geom_vline(aes(xintercept = -volatility), colour = "blue") +
  geom_label(aes(x = volatility, y = 0, label = round(volatility, 3))) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Portfolio Risk",
       subtitle = format("Value at Risk (VaR) for alpha = 5% (red) and volatility (blue)")) +
  theme_light();


#compare the method for numerical integration
alpha <- 0.05;
t.df  <- 5;
v.a.r. <- qt(p = alpha, df = t.df);
# let's see if we get something similar ...
N <- 100000;
x <- rt(n = N, df = t.df);
v.a.r.sim <- quantile(x = x, probs = alpha)
# small difference v.a.r.sim - v.a.r.

x.integrand <- function(._x_.) return(._x_. * dt(x = ._x_., df = t.df));
ex.shortfall <- (1 / alpha) * integrate(x.integrand, lower = -Inf, upper = v.a.r.)[["value"]];
cbFinance::expected_shortfall(x)

# here we see that the numerical integration and the simple equal weight average
# lead to slightly different outcomes

# now collect on our portfolio
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
  "clean_symbol" = c("MSFT", "AXA", "BASF", "BAYER", "BP", "FB", "IS3RDE", "SPPW.DE"),
  "type" = c("stock", "stock", "stock", "stock", "stock", "stock", "index", "index"),
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

data <- tq_get(x = assets$symbol, from = "2019-12-31", to = "2021-01-01") %>%
  dplyr::inner_join(assets, by = "symbol");

# data %>% dplyr::count(symbol)

daily_returns <- data %>%
  dplyr::select(symbol, date, close) %>%
  dplyr::group_by(symbol) %>%
  tidyquant::tq_mutate(select = close,
                       mutate_fun = periodReturn,
                       period = "daily", type = "log")