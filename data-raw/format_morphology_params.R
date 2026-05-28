## --------------------------------------------------------------#
## Script: format_params_df_morphology.R
## Purpose: Read morphology_model_results.xlsx, standardize column names and
##          relationship labels, and save as the params_df_morphology package
##          data object.
## Author: Paul Bzonek [Claude]
## Date Created: 2026-05-28
## --------------------------------------------------------------#

temp_morph_raw <- readxl::read_xlsx(
  "data-raw/files-raw/morphology_model_results.xlsx",
  sheet = "Model data"
)

# Standardize column names
params_df_morphology <- temp_morph_raw %>%
  dplyr::rename(
    species_common = `Species name`,
    relationship   = `Relationship`,
    n              = `n`,
    intercept_a    = `intercept_a`,
    slope_b        = `slope_b`,
    r_squared      = `R^2`,
    equation       = `Equation`
  ) %>%
  # Standardize relationship labels to snake_case keys
  dplyr::mutate(
    relationship = dplyr::case_when(
      relationship == "Width ~ Fork Length"          ~ "width_fl",
      relationship == "Mass ~ Width"                 ~ "mass_width",
      relationship == "Mass ~ Fork Length"           ~ "mass_fl",
      relationship == "log(Width) ~ log(Fork Length)" ~ "log_width_fl",
      relationship == "Width ~ Mass^b"               ~ "width_mass",
      TRUE ~ relationship
    )
  ) %>%
  as.data.frame()

rm(temp_morph_raw)

usethis::use_data(params_df_morphology, overwrite = TRUE, compress = "xz")

cat("params_df_morphology saved:\n")
cat("  Rows:", nrow(params_df_morphology), "\n")
cat("  Species:", length(unique(params_df_morphology$species_common)), "\n")
cat("  Relationships:", paste(unique(params_df_morphology$relationship), collapse = ", "), "\n")
