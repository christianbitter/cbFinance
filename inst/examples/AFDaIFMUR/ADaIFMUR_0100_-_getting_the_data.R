# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc: getting the AMZN data

rm(list = ls());
library(tidyquant);
library(tidymodels);
library(ggplot2);
library(skimr);

asset <- c("AMZN");

from <- "2010-12-31";
to   <- "2013-12-31";

# other assets
# IBM (IBM), S&P 500 Index (ˆGSPC), SPDR S&P 500 ETF (SPY), S&P 600 Small Cap ETF (SLY),
# SPDR Barclays Aggregate Bond ETF (LAG),
# SPDR Barclays High Yield Bond ETF (JNK),
# SPDR MSCI All Country World Index ex USA ETF (CWI), Tesla (TSLA), Yahoo (YHOO). For our implementation of event studies, we use data for
# Netflix (NFLX) and SPDR S&P 500 ETF (SPY) from July 20, 2012 to July 23
amzn_df <- NULL;
amzn_fp <- sprintf("inst/data/amzn_%s_to_%s.csv", from, to);

if (file.exists(amzn_fp)) {
  amzn_df <- readr::read_csv(file = amzn_fp);
} else {
  amzn_df <- tidyquant::tq_get(x=asset, from=from, to=to);
  readr::write_csv(x = amzn_df, path = amzn_fp)
}

skimr::skim(amzn_df);
summary(amzn_df);

amzn_df %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "Amazon", y = "Closing Price", x = "") +
  scale_x_date() +
  theme_tq();

# although mentioned, we do not need to sort the data chronologically, this
# was being taken care of for us.

class(amzn_df);

# this is a data frame/ tbl ... so to be able to process it with different
# packages, we will have to convert it to different types such as xts or ...
# however, the tidyquant package deals with data frames directly, so
# we will do this on a need-to-know basis.
# So, if we would need to convert it to an xts
library(xts);

dates <- amzn_df %>% dplyr::select(date) %>% .[[1]];
data  <- amzn_df %>% dplyr::select(-date, -symbol);
amzn_xts <- xts(x = data, order.by = dates);

plot(amzn_xts$close, main = "Amazon Closing Price");
plot(amzn_xts$volume, main = "Amazon Volume");