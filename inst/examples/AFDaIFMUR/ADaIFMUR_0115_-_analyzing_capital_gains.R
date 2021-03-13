# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc: getting the AMZN data

rm(list = ls());
library(tidyquant);
library(tidymodels);
library(ggplot2);
library(skimr);

source("./R/AFDaIFMUR/common.R");

# A common question when investing is to compare how our investment performed
# compared to other securities over a certain time horizon.
# here, we compare the following three assets

amzn_df <- get_asset();
gpsc_df <- get_asset(asset="^GSPC", alt_ticker="SP500");
ibm_df  <- get_asset(asset="IBM");
#
# We now pose the question, which of these investments performed better from
# December 31, 2010 through December 31, 2013 based solely on price appreciation?

amzn_df %>%
  ggplot(aes(x = date, y = close)) +
  geom_barchart(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "AMZN Bar Chart", y = "Closing Price", x = "") +
  theme_tq();

gpsc_df %>%
  ggplot(aes(x = date, y = close)) +
  geom_barchart(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "SP500 Bar Chart", y = "Closing Price", x = "") +
  theme_tq()

ibm_df %>%
  ggplot(aes(x = date, y = close)) +
  geom_barchart(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "IBM Bar Chart", y = "Closing Price", x = "") +
  theme_tq()
# We need to track the changes in the price of each security from the
# same starting value (i.e., starting from their respective closing
# prices on December 31, 2013).
#
# For the starting value, we decide to use $ 1 as the
# starting point, so it is easier to interpret the values in the chart
# as a percentage change in the price of the security.
# This chart is sometimes known as a normalized price chart.

# Now, in order to get to this, we have two options
# 1) we start with one dollar in the investment and track/ accumulate the
# changes from day to day, i.e. day 2 to day 1, day 3 to day 2 until
# the very last day or ..., i.e. we multiply the daily percentage changes
# individually until the end
#
# 2) we do not care for what happens between the last and the final date
# but only look at the difference from day 1 to day n. this is equivalent
# and we can apply this to each day, i.e. day 2 to day 1, day 3 to day 1.
# let's test this for amazon

way1 <- amzn_df$close[1:10];
way2 <- way1;
temp <- numeric(length(way1));

# a very naive for loop
temp[1] <- 1;
for (i in 2:length(way1)) {
  # cumulate the daily percent change
  temp[i] <- temp[i - 1] * ( way1[i] / way1[i - 1] );
}
temp <- temp - 1;
way1 <- temp;
plot(way1,
     main = "(1) and (2) %-change for 1st 10 days of AMZN",
     type="l", lwd=1, xlab="Day", ylab="%", col="blue")

way2 <- way2 / way2[1];
way2 <- way2 - 1;

lines(way2, col="red");
abline(h=0,lty=1,col="black");
legend("topleft", c("(1)","(2)","base"),
       col=c("blue","red", "black"), lty=c(2,2,1), lwd=c(1,1, 2));

# now replicate for the three assets
amzn_close <- amzn_df$close;
ibm_close <- ibm_df$close;
gpsc_close <- gpsc_df$close;

amzn_close <- amzn_close / amzn_close[1];
ibm_close  <- ibm_close / ibm_close[1];
gpsc_close <- gpsc_close / gpsc_close[1];

plot(amzn_close,
     main = "% Change of closing prices AMZN, IBM, SP500",
     type="l", lwd=1, xlab="Day", ylab="%", col="blue")

lines(ibm_close, col="red");
lines(gpsc_close, col="gray");
abline(h=0,lty=1,col="black");
legend("topleft", c("AMZN","IBM", "SP500", "base"),
       col=c("blue","red", "gray", "black"), lty=c(2,2,2,1), lwd=c(1,1,1,2));