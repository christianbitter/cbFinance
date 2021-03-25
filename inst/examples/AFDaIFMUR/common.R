#'@author chritian bitter
#'@name get_asset
#'@description convenience call to tq_get storing data locally if not existing
#'and pulling it.
get_asset <- function(asset = "AMZN", from = "2010-12-31", to = "2013-12-31",
                      alt_ticker = NULL) {
  library(tidyquant);

  if (asset == "^GSPC") {
    warning("asset sp500 use alt_ticker");
    alt_ticker <- "sp500";
  }

  a_df <- NULL;
  if (is.null(alt_ticker)) {
    a_fp <- sprintf("inst/data/%s_%s_to_%s.csv", asset, from, to);
  } else {
    a_fp <- sprintf("inst/data/%s_%s_to_%s.csv", alt_ticker, from, to);
  }

  if (file.exists(a_fp)) {
    a_df <- readr::read_csv(file = a_fp);
  } else {
    a_df <- tidyquant::tq_get(x = asset, from = from, to = to);
    if (is.na(a_df) || is.null(a_df) || nrow(a_df) < 1) {
      stop(sprintf("No data found for symbol: %s", asset));
    }

    readr::write_csv(x = a_df, file = a_fp);
  }

  return(a_df);
}


#'@name adjusted.close
#'@param .data a data frame containing asset adjusted close data by date, likely
#'the result of a tq_get call.
#'@author christian bitter
#'@param rename the name to be assigned to the adjusted closing price attribute
#'@return a new data frame with two columns date and the potentially renamed
#'adjusted closing price
#'@description assuming that .data represents a data frame containing a column adjusted
#'and date, project those two columns and potentially rename adjusted to rename
adjusted.close <- function(.data, rename = "adjusted") {
  return(dplyr::select(.data, date, adjusted) %>%
           dplyr::rename(!!rename := adjusted));
}

#'@name adjusted.daily.returns
#'@description assuming that .data represents a data frame containing a column adjusted
#'and date, compute daily returns from it. Furthermore, the attributes adjusted and daily return
#'are potentially renamed to names provided via adjusted and name.return
adjusted.daily.returns <- function(.data, rename = "adjusted", name.return = "Rd") {
  return(dplyr::select(.data, date, adjusted) %>%
           dplyr::rename(!!rename := adjusted) %>%
           dplyr::mutate(Rd = (adjusted / lag(adjusted)) - 1.) %>%
           dplyr::rename(!!name.return := Rd));
}

#'@name closing.price
#'@description convenience function projecting date, close and adjusted in a tq_get-style data frame.
#'if provided the attributes are renamed and prefixed
closing.price <- function(.data, prefix, close_name = "close", adjusted_name = "adjusted") {
  if (!is.null(prefix) && stringr::str_length(prefix) > 0) {
    close_name <- sprintf("%s%s", prefix, close_name);
    adjusted_name <- sprintf("%s%s", prefix, adjusted_name);
  }

  .data <- .data %>% dplyr::select(date, close, adjusted) %>%
    dplyr::rename(!!close_name := close,
                  !!adjusted_name := adjusted);
}

#'@name eom
#'@description gets the date of the last day in a month
eom <- function(date)
    lubridate::ceiling_date(lubridate::date(date), "month") - 1

ff_3f <- function(save_data = T) {
  base_dir <- "inst/data";
  monthly_fp <- file.path(base_dir, "F-F_Research_Data_Factors_monthly.csv");
  yearly_fp <- file.path(base_dir, "F-F_Research_Data_Factors_yearly.csv");

  if (file.exists(monthly_fp) & file.exists(yearly_fp)) {
    monthly_df <- readr::read_csv(monthly_fp);
    yearly_df  <- readr::read_csv(yearly_fp);

    return(list(
      "yearly" = yearly_df,
      "monthly" = monthly_df
    ));
  }

  fp <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip";
  temp <- tempfile()
  download.file(fp, temp)
  temp_dir <- tempdir();

  unzip(zipfile = temp, exdir = temp_dir);
  data_fp <- file.path(temp_dir, "F-F_Research_Data_Factors.CSV");
  data_df <- readr::read_csv(data_fp, skip_empty_rows = T, skip = 2);
  unlink(temp_dir);
  unlink(temp);

  # the original data contains the copyright in the last row, this will be empty
  # so skip it.
  data_df <- data_df[-nrow(data_df), ];

  # clean up the names
  names(data_df) <- c("date", "RmxRf", "SMB", "HML", "Rf");

  # convert date from real to int and convert rates to fractions
  data_df <- data_df %>%
    dplyr::mutate(date = as.integer(date)) %>%
    dplyr::mutate(RmxRf = RmxRf / 100.,
                  Rf = Rf / 100.);

  # the file contains monthly and yearly aggregate, so pay attention
  monthly_df <- data_df %>%
    dplyr::filter(date >= 10000) %>%
    dplyr::mutate(date = lubridate::ymd(date, truncated = 1L));

  # since the yearly data is at most 2021 - we can filter out all rows < 10000
  yearly_df <- data_df %>%
    dplyr::filter(date < 10000) %>%
    dplyr::mutate(date = lubridate::ymd(date, truncated = 2L));

  # save the data for next time?
  if (save_data) {
    readr::write_csv(x = monthly_df, path = monthly_fp);
    readr::write_csv(x = yearly_df, path = yearly_fp);
  }

  return(list(
    "yearly" = yearly_df,
    "monthly" = monthly_df
  ));
}