# auth: chritian bitter
# source: Analyzing Financial Data and Implementing Financial Models Using R
# desc: getting the AMZN data
get_asset <- function(asset = "AMZN", from = "2010-12-31", to = "2013-12-31",
                      alt_ticker = NULL) {
  library(tidyquant);

  if (asset == "^GSPC") {
    warning("asset sp500 use alt_ticker");
    alt_ticker <- "sp500";
  }

  # other assets
  # IBM (IBM), S&P 500 Index (ˆGSPC), SPDR S&P 500 ETF (SPY), S&P 600 Small Cap ETF (SLY),
  # SPDR Barclays Aggregate Bond ETF (LAG),
  # SPDR Barclays High Yield Bond ETF (JNK),
  # SPDR MSCI All Country World Index ex USA ETF (CWI), Tesla (TSLA), Yahoo (YHOO). For our implementation of event studies, we use data for
  # Netflix (NFLX) and SPDR S&P 500 ETF (SPY) from July 20, 2012 to July 23
  a_df <- NULL;
  if (is.null(alt_ticker)) {
    a_fp <- sprintf("inst/data/%s_%s_to_%s.csv", asset, from, to);
  } else {
    a_fp <- sprintf("inst/data/%s_%s_to_%s.csv", alt_ticker, from, to);
  }

  if (file.exists(a_fp)) {
    a_df <- readr::read_csv(file = a_fp);
  } else {
    a_df <- tidyquant::tq_get(x=asset, from=from, to=to);
    if (is.na(a_df) || is.null(a_df) || nrow(a_df) < 1) {
      stop(sprintf("No data found for symbol: %s", asset));
    }
    readr::write_csv(x = a_df, path = a_fp)
  }

  return(a_df);
}


adjusted.close <- function(.data, rename = "adjusted") {
  return(dplyr::select(.data, date, adjusted) %>%
           dplyr::rename(!!rename := adjusted));
}

#'@name end of month
eom <- function(date)
    lubridate::ceiling_date(lubridate::date(date), "month") - 1
