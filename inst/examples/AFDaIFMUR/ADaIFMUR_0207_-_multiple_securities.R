# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc: computing monthly returns

#https://www.investopedia.com/terms/e/ema.asp

rm(list = ls());
library(tidyquant);
library(tidymodels);
library(ggplot2);
library(skimr);
library(lubridate);

source("./R/AFDaIFMUR/common.R");

# in order to get the last day of the month use lubridate ceiling date
eom <- function(date)lubridate::ceiling_date(lubridate::date(date), "month") - 1

amzn_df <- get_asset(asset="AMZN");
ibm_df <- get_asset("IBM");
msft_df <- get_asset("MSFT");

if (nrow(amzn_df) != nrow(ibm_df) && nrow(ibm_df) != nrow(msft_df)) stop("data point mismatch");

amzn_df <- amzn_df %>% dplyr::select(date, amzn_adjusted = adjusted);
ibm_df <- ibm_df %>% dplyr::select(date, ibm_adjusted = adjusted);
msft_df <- msft_df %>% dplyr::select(date, msft_adjusted = adjusted);

amzn_df$amzn_adjusted <- amzn_df$amzn_adjusted / amzn_df$amzn_adjusted[1];
ibm_df$ibm_adjusted   <- ibm_df$ibm_adjusted / ibm_df$ibm_adjusted[1];
msft_df$msft_adjusted <- msft_df$msft_adjusted / msft_df$msft_adjusted[1];

multi_df <- amzn_df %>%
  dplyr::inner_join(ibm_df) %>%
  dplyr::inner_join(msft_df);

multi_df %>%
  ggplot() +
  geom_line(aes(x = date, y = amzn_adjusted, colour = "AMZN")) +
  geom_line(aes(x = date, y = ibm_adjusted, colour = "IBM")) +
  geom_line(aes(x = date, y = msft_adjusted, colour = "MSFT")) +
  geom_hline(aes(x = date, yintercept = 1, colour = "Base")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "date", y = "Index returns of different securities",
       title = "AMZN vs. MSFT vs. IBM",
       subtitle = "Cumulative gross return") +
  theme_tq()