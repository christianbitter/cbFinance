# name: utility
# desc: explore the concept of utility and indifference curves

rm(list = ls());

library(tidyquant);
library(tidyverse);

# the utility of an instrument i to an investor j with risk aversion
# l_j and instrument risk R_i is defined as
# U_ij = E(r_i) + l_j * R_i ^ w
# where w > 1 reflects the marginal utility of risk

# let's look at microsoft, apple and ibm
assets <- c("MSFT", "AAPL", "IBM");
data <- tidyquant::tq_get(x = assets, from = "2019-12-31", to = "2021-01-01");

daily_returns <- data %>%
  dplyr::group_by(symbol) %>%
  tidyquant::tq_mutate(select = close,
                       mutate_fun = periodReturn,
                       period = "daily", type = "log",
                       col_rename = "Rd");

# now we have the daily returns of three different portfolios, comprised
# of exactly one asset - the individual stock
# let's compute the expected return of each and plot the utility/ indifference
# curves.

# this gives us access to annualized returns and risk via stddev
anualized_performance <- daily_returns %>%
  dplyr::select(symbol, date, Rd) %>%
  dplyr::group_by(symbol) %>%
  tidyquant::tq_performance(Ra = Rd,
                            performance_fun = table.AnnualizedReturns);

# now for, fix our risk aversion l
# U_ij = E(r_i) + l_j * R_i ^ w
l_j <- .3;
w   <- 1.1;
utility <- anualized_performance %>%
  dplyr::select(symbol,
                R_annualized = AnnualizedReturn,
                Risk_i = AnnualizedStdDev) %>%
  dplyr::mutate(Utility_i = R_annualized - l_j * Risk_i^w);

utility %>%
  ggplot(aes(x = reorder(symbol, -Utility_i), y= Utility_i, fill = symbol)) +
  geom_col() +
  labs(title = "Utility of different assets", x = "Asset", y = "Utility",
       fill = "Asset",
       subtitle = paste("Under the same risk appetite/aversion, we can see that, different assets have different",
                        "utility. IBM even has negative utility, denoting it's risk outweighs the asset's return.",
                        sep = "\n")
  ) +
  theme_light()