# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc:
# To determine whether our investment is performing well, we compare the returns of
# our investment to that of a benchmark. Typically, some index of comparable securities
# are used as the benchmark.

rm(list = ls());
library(tidyquant);
library(ggplot2);
library(tidyr);
library(lubridate);

source("inst/examples/AFDaIFMUR/common.R");

# note that opposed to the book which uses YAHOO, we use MSFT because YHOO does
# not exist anymore.
amzn_df <- get_asset("AMZN") %>% closing.price(prefix = "AMZN_");
ibm_df <- get_asset("IBM") %>% closing.price(prefix = "IBM_");
msft_df <- get_asset("MSFT") %>% closing.price(prefix = "MSFT_");

all_df <- amzn_df %>%
  dplyr::inner_join(ibm_df) %>%
  dplyr::inner_join(msft_df);

# compute the daily returns based on adjusted closing prices
# and filter to data from December 31, 2012 to December 31, 2013
all_df <- all_df %>%
  tq_mutate(select = AMZN_adjusted,
            mutate_fun = periodReturn,
            period     = "daily",
            col_rename = "Rd_AMZN") %>%
  tq_mutate(select = IBM_adjusted,
            mutate_fun = periodReturn,
            period     = "daily",
            col_rename = "Rd_IBM") %>%
  tq_mutate(select = MSFT_adjusted,
            mutate_fun = periodReturn,
            period     = "daily",
            col_rename = "Rd_MSFT") %>%
  dplyr::filter(date >= "2012-12-31" & date <= "2013-12-31");

# Construct an equal weighed portfolio
# An equal-weighted (EW) index gives equal weight to small firms and large firms.
# An example of EW indexes is the S&P 500 Equal Weight Index or those provided
# by MCSI. These indexes are rebalanced quarterly, which means that in between quarters the
# constituent weights are allowed to fluctuate based on their performance.
# The quarterly rebalancing approach means that on each rebalancing date, the
# weight of each firm is set or reset to 1/N where N is the number of securities
# in the portfolio.

# project to relevant attributes, and convert daily returns to gross returns
ewp_df <- all_df %>%
  dplyr::select(date, Rd_AMZN, Rd_IBM, Rd_MSFT) %>%
  dplyr::mutate(AMZN = Rd_AMZN + 1,
                IBM = Rd_IBM + 1,
                MSFT = Rd_MSFT + 1,
                no_securities = 3) %>%
  dplyr::select(-Rd_AMZN, -Rd_IBM, -Rd_MSFT);

# now we need to look at each quarter individually, implement the rebalance
ewp_q1_df <- ewp_df %>% dplyr::filter(date <= "2013-03-31");
# so we rebalance our portfolio to the beginn of the quarter, by setting
# each gross return (or portfolio security indexed investment) to 1
ewp_q1_df[1, 2:4] <- 1.
# now we can accumulated until the end of quarter 1
ewp_q1_df <- ewp_q1_df %>%
  dplyr::mutate(cAMZN = cumprod(AMZN),
                cIBM = cumprod(IBM),
                cMSFT = cumprod(MSFT),
                idx.AMZN = cAMZN / no_securities,
                idx.IBM = cIBM / no_securities,
                idx.MSFT = cIBM / no_securities);

# The value of a portfolio is equal to the value of the components of the portfolio.
# Hence, to calculate the value of the EW portfolio on each day, we sum the values
# of the three index variables.

ewp_q1_df <- ewp_q1_df %>%
  dplyr::mutate(pval = idx.AMZN + idx.IBM + idx.MSFT);

# let's look at the value of the portfolio - here we see
# the composition effect. The portfolio may not be as good as the best asset
# on a given day, it also is not as bad as the worst, and we can benefit
# from strong candidates. However, we also see through the equal weighing
# we might be able to do better
ewp_q1_df %>%
  ggplot() +
  geom_line(aes(x = date, y = pval, colour = "Portfolio")) +
  geom_line(aes(x = date, y = cAMZN, colour = "AMZN"), linetype = "dashed") +
  geom_line(aes(x = date, y = cIBM, colour = "IBM"), linetype = "dashed") +
  geom_line(aes(x = date, y = cMSFT, colour = "MSFT"), linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "date", y = "Return (%)", colour = "Securities",
       title = "Portfolio Value",
       subtitle = "Equally weighed portfolio composed of IBM, MSFT, and AMZN for Q1-2013") +
  theme_tq();

# now, we need to do the same for the second, third and fourth quarter
# we get the last day of the previous quarter
portfolio_rebalance <- ewp_q1_df$pval[nrow(ewp_q1_df)];

# now get the data for the 2nd, 3rd and 4th quarter ready
ewp_q2_df <- ewp_df %>% dplyr::filter(date >= "2013-04-01" & date < "2013-07-01");
ewp_q3_df <- ewp_df %>% dplyr::filter(date >= "2013-07-01" & date < "2013-10-01");
ewp_q4_df <- ewp_df %>% dplyr::filter(date >= "2013-10-01" & date <= "2014-01-01");

ewp_q2_df <- ewp_q2_df %>%
  dplyr::mutate(cAMZN = cumprod(AMZN),
                cIBM = cumprod(IBM),
                cMSFT = cumprod(MSFT),
                # note that instead of rebalancing by 1/no-securities or 1/3
                # we take the volume of the portfolio of the last rebalancing date - portfolio_rebalance
                idx.AMZN = cAMZN * (portfolio_rebalance / no_securities),
                idx.IBM = cIBM *  (portfolio_rebalance / no_securities),
                idx.MSFT = cIBM * (portfolio_rebalance / no_securities),
                pval = idx.AMZN + idx.IBM + idx.MSFT);

# so what was the portfolio worth at the end - wow it dropped
portfolio_rebalance <- ewp_q2_df$pval[nrow(ewp_q2_df)];

# Q3
ewp_q3_df <- ewp_q3_df %>%
  dplyr::mutate(cAMZN = cumprod(AMZN),
                cIBM = cumprod(IBM),
                cMSFT = cumprod(MSFT),
                # note that instead of rebalancing by 1/no-securities or 1/3
                # we take the volume of the portfolio of the last rebalancing date - portfolio_rebalance
                idx.AMZN = cAMZN * (portfolio_rebalance / no_securities),
                idx.IBM = cIBM *  (portfolio_rebalance / no_securities),
                idx.MSFT = cIBM * (portfolio_rebalance / no_securities),
                pval = idx.AMZN + idx.IBM + idx.MSFT);

# so at the end of Q3 - we made a little
portfolio_rebalance <- ewp_q3_df$pval[nrow(ewp_q3_df)];

# Q4
ewp_q4_df <- ewp_q4_df %>%
  dplyr::mutate(cAMZN = cumprod(AMZN),
                cIBM = cumprod(IBM),
                cMSFT = cumprod(MSFT),
                # note that instead of rebalancing by 1/no-securities or 1/3
                # we take the volume of the portfolio of the last rebalancing date - portfolio_rebalance
                idx.AMZN = cAMZN * (portfolio_rebalance / no_securities),
                idx.IBM = cIBM *  (portfolio_rebalance / no_securities),
                idx.MSFT = cIBM * (portfolio_rebalance / no_securities),
                pval = idx.AMZN + idx.IBM + idx.MSFT);

# finally we combine and
ewp_df <- dplyr::bind_rows(ewp_q1_df, ewp_q2_df, ewp_q3_df, ewp_q4_df);

# project the data to the meaningful attributes
ewp_df <- ewp_df %>% dplyr::select(date, cAMZN, cIBM, cMSFT, pval);

# so let's draw it to get a nice overview for how we did in 2013
# through the portfolio we absorbed the shock that happened to MSFT at the end
# of Q2. Overall AMZN seems to provide the overall trend. The linear trend mode
# seems to hint at an overall small up-ward move of our portfolio.
ewp_df %>%
  ggplot() +
  geom_line(aes(x = date, y = pval, colour = "Portfolio")) +
  geom_smooth(method = "lm", aes(x = date, y = pval)) +
  geom_hline(aes(x = date, yintercept = 1), colour = "black", linetype = "dotted") +
  geom_line(aes(x = date, y = cAMZN, colour = "AMZN"), linetype = "dashed") +
  geom_line(aes(x = date, y = cIBM, colour = "IBM"), linetype = "dashed") +
  geom_line(aes(x = date, y = cMSFT, colour = "MSFT"), linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "date", y = "Return (%)", colour = "Securities",
       title = "Portfolio Value",
       subtitle = "Equally weighed portfolio composed of IBM, MSFT, and AMZN for 2013") +
  theme_light();
