# desc: compute the risk and expected return for AMZN for 2011-2013

rm(list = ls());

library(dplyr);
library(ggplot2);
library(lubridate);
library(tidyquant);

source("inst/examples/AFDaIFMUR/common.R")

# https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
# here we take the log of the product - transforming it to a sum and then finally
# doing 1/d is the same as x^(1/d)
# raising it to the e
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}

amzn_df <- get_asset(from = "2010-12-31", to = "2013-12-31") %>%
  adjusted.close() %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               col_rename = "Rd") %>%
  dplyr::mutate(Gd = Rd + 1.,
                cD = cumprod(Gd)) %>%
  dplyr::mutate(year = lubridate::year(date));

amzn_df %>%
  ggplot(aes(x = date, y = Rd)) +
  geom_line() +
  geom_hline(aes(x = date, yintercept = mean(Rd)), colour = "blue") +
  labs(x = "Date", y = "Rd", title = "AMZN - Risk",
       subtitle = "Expected daily returns and volatility of daily returns") +
  theme_tq()

# compute the overall mean, variance but also for yearly subsets
aggregate_df <- amzn_df %>%
  dplyr::filter(year > 2010) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(mu = mean(Rd),
                   sigma = var(Rd),
                   gmu = gm_mean(Rd)) %>%
  dplyr::rename("scope" = year) %>%
  dplyr::mutate(scope = as.character(scope)) %>%
  dplyr::bind_rows(
    dplyr::summarise(amzn_df,
                     mu = mean(Rd),
                     sigma = var(Rd),
                     gmu = gm_mean(Rd),
                     scope = "2010-2013")
  ) %>%
  dplyr::mutate(sd = sqrt(sigma));


# since these are based on daily returns, we cannot directly compare them
# with estimates for other time scales - standard deviations of returns based
# on different frequencies are not comparable.
# so, we have to annualize the returns/ the variance and means
# by factoring in the number of trading days that form the basis of our returns
# in our case the assumption is 252 trading days per year

d <- 252;
aggregate_annualized_df <- aggregate_df %>%
  dplyr::mutate(mu = mu * d,
                sigma = sigma * d,
                sd = sd * sqrt(d))

# let's visualize
aggregate_annualized_df %>%
  tidyr::pivot_longer(cols = c("mu", "sigma", "sd"), names_to = "measure") %>%
  ggplot(aes(x = scope, y = value, fill = scope)) +
  geom_col() +
  theme_tq() +
  labs(title = "AMZN - Risk Analysis", fill = "Time",
       subtitle = "Individual security analysis comprised of annualized expected returns and volatility") +
  facet_wrap(measure ~ .)