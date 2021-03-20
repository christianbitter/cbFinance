# desc: download the Fama/French data
# the data is Fama/French 3 Factors
# under
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html

rm(list = ls());

#'@title Auxiliary Data
#'@name ff_3f
#'@author Christian Bitter
#'@description Downloads the Fama French 3 Factor data set and structures it as
#'monthly and yearly components. Each component provides the same factors
#'More information about the data can be found Rm-Rf (the excess return on the market, value-weight return),
#'SMB (Small minus big), HML (High minus Low) and Rf (risk free rate).
#'here: \url{https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_factors.html}.
#'@returns A list with two entries yearly and monthly representing the FF yearly
#'and monthly aggregates.
#'@export
ff_3f <- function() {
  library(readr);
  library(lubridate);

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

  # convert date from real to int
  data_df <- data_df %>% dplyr::mutate(date = as.integer(date));

  # the file contains monthly and yearly aggregate, so pay attention
  monthly_df <- data_df %>%
    dplyr::filter(date >= 10000) %>%
    dplyr::mutate(date = lubridate::ymd(date, truncated = 1L));

  # since the yearly data is at most 2021 - we can filter out all rows < 10000
  yearly_df <- data_df %>%
    dplyr::filter(date < 10000) %>%
    dplyr::mutate(date = lubridate::ymd(date, truncated = 2L));

  return(list(
    "yearly" = yearly_df,
    "monthly" = monthly_df
  ));
}

ff_3f_df <- ff_3f();

skimr::skim(ff_3f_df$yearly);
skimr::skim(ff_3f_df$monthly);