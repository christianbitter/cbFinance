# desc: download the Fama/French data
# the data is Fama/French 3 Factors
# under
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html

rm(list = ls());
library(tidyquant);
library(ggplot2);
library(tidyr);
library(readr);
library(lubridate);

ff_3f <- function() {
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
  # so skip it
  data_df <- data_df[-nrow(data_df), ];
  # clean up the names
  names(data_df) <- c("text.date", "RmxRf", "SMB", "HML", "Rf");
  return(data_df);
}

ff_3f_df <- ff_3f();

skimr::skim(ff_3f_df);