## --------------------------------------------------------------#
## Script: format_growth_params.R
## Purpose: Read the Parameters sheet from vb_growth_params.xlsx and save
##          as the vb_growth_params package data object.
##          Run this script after adding new species rows to the Excel file.
## Author: Paul Bzonek [Claude]
## Date Created: 2026-05-28
## --------------------------------------------------------------#

params_df_VonBertalanffy <- readxl::read_xlsx(
  "data-raw/files-raw/vb_growth_params.xlsx",
  sheet = "Parameters"
) %>%
  as.data.frame()

usethis::use_data(params_df_VonBertalanffy, overwrite = TRUE, compress = "xz")

cat("params_df_VonBertalanffy saved:\n")
cat("  Species:", nrow(params_df_VonBertalanffy), "\n")
cat("  Columns:", paste(names(params_df_VonBertalanffy), collapse = ", "), "\n")
