#' download_data.R
#'
#' Downloads the data sets for group assignment 1
#' of Social Media and Web Analytics at TiSEM in 2021
#' 

# --- Load Libraries --- #
library(googledrive)

# --- Info --- #
data_id <- "1qIta7U7CU7RnvPdVI1FXPX8dZdiJXl3g"
out_file <- "data.zip"

#---- Download --- #
drive_download(
  as_id(data_id), 
  path = out_file, 
  overwrite = TRUE)

# --- Unzip and Clean up --- #
unzip(out_file,
      exdir = "data")

file.remove(out_file)
