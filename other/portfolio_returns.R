# based on: https://rviews.rstudio.com/2017/10/11/from-asset-to-portfolio-returns/
rm(list = ls())

library(tidyquant)
library(ggplot2)
library(dplyr)
library(readr)

# + SPY (S&P500 fund) weighted 25%
# + EFA (a non-US equities fund) weighted 25%
# + IJS (a small-cap value fund) weighted 20%
# + EEM (an emerging-mkts fund) weighted 20%
# + AGG (a bond fund) weighted 10%

portfolio_df <-
  tibble(
    "symbol" = c("SPY", "EFA", "IJS", "EEM", "AGG"),
    "w" = c(.25, .25, .2, .2, .1)
  )

prices <- tidyquant::tq_get(portfolio_df$symbol,
                            from = "2013-01-01",
                            to = "2017-01-01")

prices %>% dplyr::count(symbol)
# equal number of observations

# compute monthly log returns for each asset
lRet_m <- prices %>%
  dplyr::group_by(symbol) %>%
  tidyquant::tq_transmute(
    select = close,
    mutate_fun = periodReturn,
    period = "daily",
    type = "log",
    col_rename = "Rm"
  )

lRet_m %>%
  ggplot(aes(x = date, y = Rm, color = symbol)) +
  geom_line() +
  tidyquant::theme_tq() +
  labs(x = "Month", y = "Log Return", title = "Monthly Log Returns of Assets",
       colour = "Asset") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date() +
  facet_wrap(symbol ~ .)

# build our portfolio returns by incorporating the weights
lRet_p <- lRet_m %>%
  tq_portfolio(
    assets_col  = symbol,
    returns_col = Rm,
    weights     = portfolio_df,
    col_rename  = "Rp",
    rebalance_on = "months"
  )

lRet_p %>%
  ggplot(aes(x = date, y = Rp)) +
  geom_line() +
  tidyquant::theme_tq() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date() +
  labs(x = "Month", y = "Log Return", title = "Portfolio Monthly Log Returns")

lRet_p$CRm <- lRet_p$Rp;
lRet_p$CRm[1] <- 0.;

lRet_p <- lRet_p %>% dplyr::mutate(CRm = cumsum(CRm));

lRet_p %>%
  ggplot(aes(x = date, y = CRm)) +
  geom_line() +
  tidyquant::theme_tq() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date() +
  labs(x = "Month", y = "Log Return", title = "Portfolio Cumulative Monthly Log Returns");