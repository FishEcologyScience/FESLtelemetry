## --------------------------------------------------------------#
## Template: template00-XX_load_and_filter
## Title: Load atel Object and Apply Optional Filters
##
## Purpose:
##    Entry-point template for all FESLtelemetry analyses. Loads a
##    positionRtools atel object and optionally applies filter parameters
##    exported from glatos_exploreR. Produces a clean atel_obj ready for
##    downstream templates (template01+).
##
##    Run this template first in every FESL analysis session.
##
## --------------------------------------------------------------#
## INPUTS (choose one loading path below):
##   Path A - atel RDS file (standard): pre-processed atel object from positionRtools
##   Path B - atel from Parquet files: raw positionRtools Parquet output directory
##   Path C - filter bookmark (optional add-on): filter_bookmark.json from glatos_exploreR
##
## OUTPUTS:
##   atel_obj - Cleaned atel object ready for template01+ analysis
##
## DEPENDENCIES:
##   Required: positionRtools
##   Optional (Path C): jsonlite
##
## --------------------------------------------------------------#
## Author: [Your Name]
##
## Date Created: [Date]
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#

library(tidyverse)



##### Path A: Load atel from RDS (standard) ######################----
#-------------------------------------------------------------#
# Use this path 9/10 times — load the full atel object produced by positionRtools

cat("\n--- Loading atel object ---\n")

# param_atel_path <- "path/to/your_atel.rds"   # <-- set your path here
atel_obj <- readRDS(param_atel_path)

cat("  Loaded atel object\n")
cat("  Detections:", format(nrow(atel_obj$detections), big.mark = ","), "\n")
cat("  Animals:", nrow(atel_obj$animals), "\n")
cat("  Stations:", length(unique(atel_obj$detections$receiver_sn)), "\n")
cat("  Date range:",
    format(min(atel_obj$detections$detection_datetime_utc), "%Y-%m-%d"), "to",
    format(max(atel_obj$detections$detection_datetime_utc), "%Y-%m-%d"), "\n")



##### Path B: Load atel from Parquet (alternative) ###############----
#-------------------------------------------------------------#
# Use this path when working directly from positionRtools Parquet output files
# Comment out Path A above before using this block

# param_parquet_dir <- "path/to/parquet/folder/"   # <-- set your path here
#
# atel_obj <- positionRtools::read_atel(param_parquet_dir)
#
# cat("  Loaded atel object from Parquet\n")



##### Optional: Apply filter bookmark from glatos_exploreR #######----
#-------------------------------------------------------------#
# Use this block when you exported a filter_bookmark.json from glatos_exploreR
# and want to re-apply the same species/lake/project/date filters here.
# Leave this block commented out when using the full atel object (Path A standard).

# param_bookmark_path <- "path/to/filter_bookmark.json"   # <-- set your path here
#
# temp_filters <- jsonlite::fromJSON(param_bookmark_path)
# cat("\n--- Applying glatos_exploreR filter bookmark ---\n")
# cat("  Exported from glatos_exploreR on:", temp_filters$export_datetime, "\n")
# cat("  Filters: species =", paste(temp_filters$filters$species, collapse=", "), "\n")
# cat("           lakes   =", paste(temp_filters$filters$lakes,   collapse=", "), "\n")
# cat("           projects=", paste(temp_filters$filters$projects, collapse=", "), "\n")
# cat("           dates   =", temp_filters$filters$date_start, "to",
#                              temp_filters$filters$date_end, "\n")
#
# # --- Filter by species ---
# temp_target_animals <- dplyr::filter(
#   atel_obj$animals,
#   species_common %in% temp_filters$filters$species
# )
# atel_obj <- positionRtools::filter_custom(
#   atel_obj,
#   animal_id %in% temp_target_animals$animal_id
# )
#
# # --- Filter by date range ---
# atel_obj <- positionRtools::filter_custom(
#   atel_obj,
#   detection_datetime_utc >= as.POSIXct(temp_filters$filters$date_start, tz = "UTC"),
#   detection_datetime_utc <= as.POSIXct(temp_filters$filters$date_end,   tz = "UTC")
# )
#
# # --- Filter by project (if project codes are present in animals table) ---
# # temp_project_animals <- dplyr::filter(
# #   atel_obj$animals,
# #   project_code %in% temp_filters$filters$projects
# # )
# # atel_obj <- positionRtools::filter_custom(
# #   atel_obj,
# #   animal_id %in% temp_project_animals$animal_id
# # )
#
# cat("  Filter bookmark applied\n")
# cat("  Detections remaining:", format(nrow(atel_obj$detections), big.mark = ","), "\n")
# cat("  Animals remaining:", nrow(atel_obj$animals), "\n")
#
# rm(list = ls(pattern = "^temp_"))



##### Confirm atel object #########################################----
#-------------------------------------------------------------#
cat("\n--- atel object ready for analysis ---\n")
print(summary(atel_obj))



##### Next Step ###################################################----
#-------------------------------------------------------------#
# atel_obj is ready. Proceed to:
#   template01-XX_network_residency.R
#
# Pass atel_obj forward — no need to re-load in downstream templates.



##### Cleanup #####################################################----
#-------------------------------------------------------------#
rm(list = ls(pattern = "^temp_"))
cat("Cleanup complete.\n")
