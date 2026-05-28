## --------------------------------------------------------------#
## Script: format_growth_params.R
## Purpose: Read vb_growth_params.csv and save as package data object.
##          Run this script to update the vb_growth_params dataset after
##          adding new species rows to the CSV.
## Author: Paul Bzonek [Claude]
## Date Created: 2026-05-28
## --------------------------------------------------------------#

vb_growth_params <- utils::read.csv(
  "data-raw/vb_growth_params.csv",
  stringsAsFactors = FALSE
)

usethis::use_data(vb_growth_params, overwrite = TRUE, compress = "xz")

cat("vb_growth_params saved:\n")
cat("  Species:", nrow(vb_growth_params), "\n")
cat("  Columns:", paste(names(vb_growth_params), collapse = ", "), "\n")
