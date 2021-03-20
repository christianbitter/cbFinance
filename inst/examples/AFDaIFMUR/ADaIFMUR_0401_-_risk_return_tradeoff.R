# desc: download the Fama/French data
# the data is Fama/French 3 Factors
# under
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html

rm(list = ls());

library(dplyr);
library(ggplot2);
library(lubridate);

source("inst/examples/AFDaIFMUR/common.R")

ff <- ff_3f()$monthly;

# Rm-Rf, the excess return on the market, value-weight return of all CRSP firms
# incorporated in the US and listed on the NYSE, AMEX, or NASDAQ that have a
# CRSP share code of 10 or 11 at the beginning of month t, good shares and price
# data at the beginning of t, and good return data for t minus the one-month
# Treasury bill rate (from Ibbotson Associates).

# the RmxRf data is the market excess return, i.e. return minus risk free rate
# so we add the risk free rate back into the data.
# The risk free rate rf is provided as Rf.
# As mentioned the short term maturity government bonds are typically the
# best representation of risk-free rate.

ff <- ff %>% dplyr::mutate(Rm = RmxRf + Rf);
# next get data from 1963 to 2013
ff <- ff %>% dplyr::filter(date >= "1963-12-01" & date < "2014-01-01")
# in order to calculate gross returns we add 1 to Rf and Rm
# furthermore to start our computation we set the first entry 1963 December to 1
ff <- ff %>% dplyr::mutate(Gf = Rf + 1,
                           Gm = Rm + 1);
ff$Gf[1] <- 1;
ff$Gm[1] <- 1;

# let's compute cumulative returns for the stock market Gm and the bonds Gf
ff <- ff %>% dplyr::mutate(cGf = cumprod(Gf),
                           cGm = cumprod(Gm));
# finally, plot the data

ff %>%
  ggplot() +
  geom_line(aes(x = date, y = cGf, colour = "Bonds"), linetype = 4) +
  geom_line(aes(x = date, y = cGm, colour = "Stocks"), linetype = 4) +
  labs(title = "Fama French 3F",
       subtitle = "Comparison of cumulative gross returns of bonds and stocks",
       y = "R ($)", x = "Date", colour = "security") +
  theme_tq();