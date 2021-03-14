rm(list = ls());

library(tidyquant);
library(readr);
library(dplyr);
library(cbFinance);

data <- readr::read_csv("inst/data/MSFT_2020.csv");
daily_returns <- data %>% tq_transmute(select     = adjusted,
                                       mutate_fun = periodReturn,
                                       period     = "daily",
                                       col_rename = "Ra");
daily_returns %>% tq_performance(
  Ra = Ra,
  Rb = NULL,
  performance_fun = VaR
);
# the difference in results can probably be explained by the default method
# chose from the performance analytics package
cbFinance::value_at_risk(daily_returns$Ra)

alpha <- 0.05;
sorted_returns <- sort(daily_returns$Ra);
frac_index <- length(sorted_returns) * alpha;
left_index <- floor(alpha_index);
right_index <- ceiling(alpha_index);
r_frac <- 1 - (frac_index - left_index);

vl <- sorted_returns[left_index];
vr <- sorted_returns[right_index];
# weighted interpolation to account for left and right
v <- (1 - r_frac) * sorted_returns[left_index] + r_frac * sorted_returns[right_index];
# quantile function uses the same ... Q[i](p) = (1 - γ) x[j] + γ x[j+1],
# linear interpolation between order statistics
ve <- sorted_returns[left_index:(left_index + 1)] %*% c(1 - r_frac, r_frac);

# the difference in results can probably be explained by the way
# the quantile function chooses order statistics