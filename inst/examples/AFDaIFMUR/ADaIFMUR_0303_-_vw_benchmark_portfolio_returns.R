# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc:
# An alternative way to construct a portfolio of securities is to use value-weighted
# returns or capitalization-weighted returns. Here, returns of larger firms have more weight.
# Weight of security i is equal to the market capitalization of
# security i divided by the market capitalization of all securities in the portfolio.

rm(list = ls());

options(digits = 5);

library(tidyquant);
library(ggplot2);
library(tidyr);
library(lubridate);
library(dplyr);
library(tibble);
library(quantmod);

source("inst/examples/AFDaIFMUR/common.R");

# note that opposed to the book which uses YAHOO, we use MSFT because YHOO does
# not exist anymore.
amzn_df <- get_asset("AMZN") %>% closing.price(prefix = "AMZN_");
ibm_df <- get_asset("IBM") %>% closing.price(prefix = "IBM_");
msft_df <- get_asset("MSFT") %>% closing.price(prefix = "MSFT_");

all_df <- amzn_df %>%
  dplyr::inner_join(ibm_df) %>%
  dplyr::inner_join(msft_df) %>%
  dplyr::filter(date >= "2012-12-31");

# Construct a value-weighed portfolio
# For our purposes, the weight of security i is equal to the market capitalization of
# security i divided by the market capitalization of all securities in the portfolio. In
# addition, we only rebalance the weights at the start of each quarter using the prior
# quarter end’s market capitalization data. The reason we use past data is because
# we want to avoid any look-ahead bias.

# since, the value-weighed approach defines value of a security as the market capitalization
# we need to determine the number outstanding shares and their price for a given
# market day. Which means, we also need to determine what a market day is.
# the price is available to us with the daily closing, and we simply extrapolate
# through time, i.e. where there is no trading day, we use the value of the last trading day.
# for example, we would use the value of the 31.12.2012 for the 01.01.2013

all_days <- seq(as.Date("2012-12-31"), as.Date("2013-12-31"), by = "days");
all_dates <- tibble("date" = all_days);
all_prices <- all_dates %>% dplyr::left_join(all_df);

# now we need to fill the closing prices, unfortunately, a mutate with the
# context and lag is not so simple, since the modification of a cell does not look back
# to a potential previous modification, which is necessary, if for example we have
# two consecutive non-trading days.

# so we start simple, and later turn this into a dplyr function
for (i in 1:nrow(all_prices)) {
  all_prices$AMZN_close[i] <- ifelse(is.na(all_prices$AMZN_close[i]), all_prices$AMZN_close[i - 1], all_prices$AMZN_close[i]);
  all_prices$IBM_close[i] <- ifelse(is.na(all_prices$IBM_close[i]), all_prices$IBM_close[i - 1], all_prices$IBM_close[i]);
  all_prices$MSFT_close[i] <- ifelse(is.na(all_prices$MSFT_close[i]), all_prices$MSFT_close[i - 1], all_prices$MSFT_close[i]);

  all_prices$AMZN_adjusted[i] <- ifelse(is.na(all_prices$AMZN_adjusted[i]), all_prices$AMZN_adjusted[i - 1], all_prices$AMZN_adjusted[i]);
  all_prices$IBM_adjusted[i] <- ifelse(is.na(all_prices$IBM_adjusted[i]), all_prices$IBM_adjusted[i - 1], all_prices$IBM_adjusted[i]);
  all_prices$MSFT_adjusted[i] <- ifelse(is.na(all_prices$MSFT_adjusted[i]), all_prices$MSFT_adjusted[i - 1], all_prices$MSFT_adjusted[i]);
}

# get the end-quarter data

qtr_prices <- all_prices %>%
  dplyr::filter(
    date == "2012-12-31" | date == "2013-03-31" | date ==  "2013-06-30" | date == "2013-09-30"
  )

# Obtain Shares Outstanding Data from SEC Filings.
# https://www.sec.gov/edgar/searchedgar/companysearch.html
# The quarter-end shares outstanding are obtained from the SEC EDGAR database
AMZN.shout <- c(454000000,455000000,457000000,458000000);
IBM.shout <- c(1117367676,1108794396,1095425823,1085854383);
# for now we use the YHOO values as the MSFT values
MSFT.shout <- c(1117367676,1108794396,1095425823,1085854383);


qtr_prices <- qtr_prices %>% dplyr::bind_cols("AMZN_shout" = AMZN.shout,
                                              "IBM_shout"  = IBM.shout,
                                              "MSFT_shout" = MSFT.shout) %>%
  # now calculate the market cap at the end of the quarter for each security
  dplyr::mutate(AMZN_mcap = AMZN_shout * AMZN_close,
                IBM_mcap = IBM_shout * IBM_close,
                MSFT_mcap = MSFT_shout * MSFT_close) %>%
  # calculate the "index" cap.
  dplyr::mutate(total_cap = AMZN_mcap + IBM_mcap + MSFT_mcap) %>%
  # and weigh security by total cap to construct the volume weights
  dplyr::mutate(AMZN_wt = AMZN_mcap / total_cap,
                IBM_wt = IBM_mcap / total_cap,
                MSFT_wt = MSFT_mcap / total_cap);

# now we can construct or data frame of weights by simply projecting
qtr_weights <- qtr_prices %>% dplyr::select(date, AMZN_wt, IBM_wt, MSFT_wt);
# as mentioned earlier, the rebalancing occurs at the start of the quarter,
# with the prior quarter's end value. since the end quarter value holds also
# for the start of the quarter, we can simply roll forward one day in time
qtr_weights <- qtr_weights %>% dplyr::mutate(date = date + 1);

# note that we have the weights for the end of the quarter but need them
# for the full calendar
all_weights <- tibble("date" = all_days);
all_weights <- all_weights %>% dplyr::left_join(qtr_weights);
all_weights <- na.locf(all_weights)

# get the beginning of quarter weights
vwp_q1_w_df <- all_weights %>% dplyr::filter(date == "2013-01-01");
vwp_q2_w_df <- all_weights %>% dplyr::filter(date == "2013-04-01");
vwp_q3_w_df <- all_weights %>% dplyr::filter(date == "2013-07-01");
vwp_q4_w_df <- all_weights %>% dplyr::filter(date == "2013-10-01");

# for summary purpose get a feeling for the composition of the portfolio
dplyr::bind_rows(vwp_q1_w_df,
                 vwp_q2_w_df,
                 vwp_q3_w_df,
                 vwp_q4_w_df) %>%
  tidyr::pivot_longer(cols = c("AMZN_wt", "IBM_wt", "MSFT_wt"),
                      names_to = "symbol", values_to = "wt") %>%
  ggplot(aes(x = date, y = wt, fill = symbol)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "symbol", y = "weight", title = "Portfolio",
       subtitle = "Portfolio composition of AMZN, IBM and MSFT stock") +
  theme_tq();


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
            col_rename = "Rd_MSFT");

all_df <- all_df %>%
  dplyr::mutate(AMZN = Rd_AMZN + 1,
                IBM = Rd_IBM + 1,
                MSFT = Rd_MSFT + 1) %>%
  dplyr::select(date, AMZN, IBM, MSFT);

all_df[1, c("AMZN", "IBM", "MSFT")] <- 1.;
all_df <- all_df %>% dplyr::mutate(cAMZN = cumprod(AMZN),
                                   cIBM  = cumprod(IBM),
                                   cMSFT = cumprod(MSFT));

vwp_df <- all_df %>% dplyr::select(date,
                                   AMZN = cAMZN,
                                   IBM = cIBM,
                                   MSFT = cMSFT);

vwp_q1_df <- vwp_df %>% dplyr::filter(date < "2013-04-01");

# now we can accumulate until the end of quarter 1 and balance by the weight
vwp_q1_df <- vwp_q1_df %>%
  dplyr::mutate(idx.AMZN = AMZN * vwp_q1_w_df$AMZN_wt,
                idx.IBM = IBM * vwp_q1_w_df$IBM_wt,
                idx.MSFT = MSFT * vwp_q1_w_df$MSFT_wt);

# Sum the daily index value into the daily portfolio value
# note that there is a slight difference to the values in the book
# but the process seems right
vwp_q1_df <- vwp_q1_df %>% dplyr::mutate(pval = idx.AMZN + idx.IBM + idx.MSFT);

# look at our portfolio's first quarter here ...
vwp_q1_df %>%
  ggplot() +
  geom_line(aes(x = date, y = pval, colour = "Portfolio")) +
  geom_line(aes(x = date, y = AMZN, colour = "AMZN"), linetype = "dashed") +
  geom_line(aes(x = date, y = IBM, colour = "IBM"), linetype = "dashed") +
  geom_line(aes(x = date, y = MSFT, colour = "MSFT"), linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "date", y = "Return (%)", colour = "Securities",
       title = "Portfolio Value",
       subtitle = "Value weighed portfolio composed of IBM, MSFT, and AMZN for Q1-2013") +
  theme_tq();

# TODO: q2-4
# now, we need to do the same for the second, third and fourth quarter
# we get the last day of the previous quarter
portfolio_rebalance <- vwp_q1_df$pval[nrow(vwp_q1_df)];

# now get the data for the 2nd, 3rd and 4th quarter ready
vwp_q2_df <- vwp_df %>% dplyr::filter(date >= "2013-04-01" & date < "2013-07-01");
vwp_q3_df <- vwp_df %>% dplyr::filter(date >= "2013-07-01" & date < "2013-10-01");
vwp_q4_df <- vwp_df %>% dplyr::filter(date >= "2013-10-01" & date <= "2014-01-01");


# now we can accumulate until the end of quarter 2 and balance by the weight
# NOTE: we apply the rebalance
vwp_q2_df <- vwp_q2_df %>%
  dplyr::mutate(idx.AMZN = AMZN * vwp_q2_w_df$AMZN_wt * portfolio_rebalance,
                idx.IBM = IBM * vwp_q2_w_df$IBM_wt * portfolio_rebalance,
                idx.MSFT = MSFT * vwp_q2_w_df$MSFT_wt * portfolio_rebalance);

vwp_q2_df <- vwp_q2_df %>% dplyr::mutate(pval = idx.AMZN + idx.IBM + idx.MSFT);

# Q3:
portfolio_rebalance <- vwp_q2_df$pval[nrow(vwp_q2_df)];
vwp_q3_df <- vwp_q3_df %>%
  dplyr::mutate(idx.AMZN = AMZN * vwp_q3_w_df$AMZN_wt * portfolio_rebalance,
                idx.IBM = IBM * vwp_q3_w_df$IBM_wt * portfolio_rebalance,
                idx.MSFT = MSFT * vwp_q3_w_df$MSFT_wt * portfolio_rebalance);

vwp_q3_df <- vwp_q3_df %>% dplyr::mutate(pval = idx.AMZN + idx.IBM + idx.MSFT);

# Q4:
portfolio_rebalance <- vwp_q3_df$pval[nrow(vwp_q3_df)];
vwp_q4_df <- vwp_q4_df %>%
  dplyr::mutate(idx.AMZN = AMZN * vwp_q4_w_df$AMZN_wt * portfolio_rebalance,
                idx.IBM = IBM * vwp_q4_w_df$IBM_wt * portfolio_rebalance,
                idx.MSFT = MSFT * vwp_q4_w_df$MSFT_wt * portfolio_rebalance);

vwp_q4_df <- vwp_q4_df %>% dplyr::mutate(pval = idx.AMZN + idx.IBM + idx.MSFT);

# finally we combine and
vwp_df <- dplyr::bind_rows(vwp_q1_df, vwp_q2_df, vwp_q3_df, vwp_q4_df);

# project the data to the meaningful attributes
vwp_df <- vwp_df %>% dplyr::select(date, AMZN, IBM, MSFT, pval);


# Here, we do not match the numbers of the book but the process seems right.
vwp_df %>%
  ggplot() +
  geom_line(aes(x = date, y = pval, colour = "Portfolio")) +
  geom_smooth(method = "lm", aes(x = date, y = pval), alpha = .2, colour="lightblue") +
  geom_hline(aes(x = date, yintercept = 1), colour = "black", linetype = "dotted") +
  geom_line(aes(x = date, y = AMZN, colour = "AMZN"), linetype = "dashed") +
  geom_line(aes(x = date, y = IBM, colour = "IBM"), linetype = "dashed") +
  geom_line(aes(x = date, y = MSFT, colour = "MSFT"), linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "date", y = "Return (%)", colour = "Securities",
       title = "Portfolio Value",
       subtitle = "Value-weighed portfolio composed of IBM, MSFT, and AMZN for 2013") +
  theme_light();
