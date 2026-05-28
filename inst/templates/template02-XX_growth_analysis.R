## --------------------------------------------------------------#
## Template: template02-XX_growth_analysis
## Title: Von Bertalanffy Growth Analysis
##
## Purpose:
##    Estimate fish age at tagging from fork length using the von Bertalanffy
##    growth equation, then project predicted age and fork length forward for
##    each year post-tagging. Optionally predicts body width and mass using
##    species-specific morphology models.
##
##    Two input paths are provided — choose one and comment out the other.
##
## --------------------------------------------------------------#
## INPUTS:
##   PATH A (atel, recommended):
##     atel_obj - positionRtools atel object (from template00)
##       Required atel animals columns:
##         - animal_id: Fish identifier
##         - release_date: Tagging/release date (Date or POSIXct)
##         - length_fork: Fork length at tagging (mm)
##
##   PATH B (raw dataframe):
##     data_fish - Per-fish data frame (one row per animal)
##       Required columns: animal_id, release_date (or tag_date), length_fork
##
##   Both paths:
##     df_residency (optional) - Output of template01; used to join growth
##       metrics to the per-fish-year detection summary
##
## OUTPUTS:
##   data_fish_growth - Per-fish table with age_at_tag and tag_year
##   df_growth        - Per-fish-year table with years_since_tag, age_pred,
##                      predicted_FL (and optionally predicted width/mass)
##   plots            - List of figure-ready ggplot objects:
##     $growth_trajectory - FL over detection years (observed at tag + projected)
##
## DEPENDENCIES:
##   Required packages:
##     - tidyverse (dplyr, ggplot2)
##     - FESLtelemetry
##     - positionRtools (PATH A only)
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

# Select input path: "atel" or "raw"
param_input_path <- "atel"

# Species name matching vb_growth_params (run FESLtelemetry::vb_growth_params to see options)
param_species <- "Walleye"

# Minimum age to be considered mature (used for optional maturity filter)
param_age_maturity <- 2

# Initialize plots list
plots <- list()



##### Step 1: Age at Tagging ###############################----
#-------------------------------------------------------------#
cat("\n--- Calculating age at tagging ---\n")

## PATH A: atel input ------------------------------------------#
if (param_input_path == "atel") {

  # calculate_age_at_tagging() accepts atel directly (uses $animals)
  data_fish_growth <- FESLtelemetry::calculate_age_at_tagging(
    data    = atel_obj,
    species = param_species
  )

}

## PATH B: raw dataframe ---------------------------------------#
# if (param_input_path == "raw") {
#
#   data_fish_growth <- FESLtelemetry::calculate_age_at_tagging(
#     data            = data_fish,
#     species         = param_species,
#     fork_length_col = "length_fork",
#     tag_date_col    = "release_date"
#   )
#
# }

cat("  Animals:", nrow(data_fish_growth), "\n")
cat("  Age at tag: mean =", round(mean(data_fish_growth$age_at_tag, na.rm = TRUE), 1),
    " SD =", round(sd(data_fish_growth$age_at_tag, na.rm = TRUE), 1), "years\n")
cat("  FL at tag: mean =", round(mean(data_fish_growth$length_fork, na.rm = TRUE), 0),
    " SD =", round(sd(data_fish_growth$length_fork, na.rm = TRUE), 0), "mm\n")



##### Step 2: Join Growth to Per-Fish-Year Summary #########----
#-------------------------------------------------------------#
# This step assumes df_residency exists from template01, or that you have
# a per-fish-year summary dataframe (one row per animal per detection year).
#
# If you don't have df_residency, comment this block and supply your own
# per-fish-year dataframe as df_behaviour_summary.

cat("\n--- Joining growth data to per-fish-year summary ---\n")

df_growth <- df_residency %>%
  dplyr::mutate(year = as.integer(format(date, "%Y"))) %>%
  dplyr::select(animal_id, year) %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    dplyr::select(data_fish_growth, animal_id, tag_year, age_at_tag, length_fork),
    by = "animal_id"
  )

cat("  Fish-year rows:", nrow(df_growth), "\n")



##### Step 3: Project Age and Fork Length Forward ##########----
#-------------------------------------------------------------#
cat("\n--- Projecting age and fork length forward ---\n")

df_growth <- FESLtelemetry::project_growth_forward(
  data               = df_growth,
  species            = param_species,
  detection_year_col = "year"
)

cat("  Projected age range:", round(min(df_growth$age_pred, na.rm = TRUE), 1),
    "to", round(max(df_growth$age_pred, na.rm = TRUE), 1), "years\n")
cat("  Projected FL range:", round(min(df_growth$predicted_FL, na.rm = TRUE), 0),
    "to", round(max(df_growth$predicted_FL, na.rm = TRUE), 0), "mm\n")



##### Optional Step 4: Morphology Predictions #############----
#-------------------------------------------------------------#
# Uncomment to add predicted width and mass from fork length.
# Requires the species to be present in morphology_params.

# df_growth <- df_growth %>%
#   dplyr::mutate(
#     predicted_width = FESLtelemetry::predict_morphology(
#       predicted_FL, species = param_species, relationship = "width_fl"
#     ),
#     predicted_mass = FESLtelemetry::predict_morphology(
#       predicted_FL, species = param_species, relationship = "mass_fl"
#     )
#   )

# cat("  Morphology predictions added: width (mm), mass (g)\n")



##### Growth Trajectory Plot ###############################----
#-------------------------------------------------------------#
cat("\n--- Building growth trajectory plot ---\n")

# Build trajectory data: observed FL at tagging + projected FL for detection years
temp_traj_lines <- dplyr::bind_rows(
  # Observed FL at tagging
  data_fish_growth %>%
    dplyr::transmute(animal_id, year = tag_year, FL = length_fork, tag_year),
  # Projected FL for each detection year
  df_growth %>%
    dplyr::select(animal_id, year, FL = predicted_FL, tag_year)
) %>%
  dplyr::distinct(animal_id, year, .keep_all = TRUE) %>%
  dplyr::arrange(animal_id, year)

plots$growth_trajectory <- temp_traj_lines %>%
  ggplot2::ggplot(aes(x = year, y = FL, group = animal_id, colour = as.factor(animal_id))) +
  ggplot2::geom_line(alpha = 0.5, linewidth = 0.6) +
  ggplot2::geom_point(
    data = dplyr::filter(temp_traj_lines, year == tag_year),
    aes(shape = "Observed at tagging"),
    size = 2.5, colour = "black"
  ) +
  ggplot2::scale_shape_manual(values = c("Observed at tagging" = 18)) +
  ggplot2::labs(
    x      = "Year",
    y      = "Fork length (mm)",
    colour = "Animal ID",
    shape  = NULL,
    title  = paste0(param_species, " — Growth Trajectories")
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "right")

cat("  Growth trajectory plot created\n")

rm(list = ls(pattern = "^temp_"))



##### Summary Statistics ###################################----
#-------------------------------------------------------------#
cat("\n--- SUMMARY STATISTICS ---\n")

temp_summary_growth <- df_growth %>%
  dplyr::group_by(animal_id) %>%
  dplyr::summarize(
    tag_year    = dplyr::first(tag_year),
    age_at_tag  = dplyr::first(age_at_tag),
    length_fork = dplyr::first(length_fork),
    n_years     = dplyr::n(),
    age_final   = max(age_pred,     na.rm = TRUE),
    fl_final    = max(predicted_FL, na.rm = TRUE),
    .groups     = "drop"
  )

cat("  Fish with growth projections:", nrow(temp_summary_growth), "\n")
cat("  Age at tag: mean =", round(mean(temp_summary_growth$age_at_tag, na.rm = TRUE), 1),
    " SD =", round(sd(temp_summary_growth$age_at_tag, na.rm = TRUE), 1), "years\n")
cat("  FL at tag: mean =", round(mean(temp_summary_growth$length_fork, na.rm = TRUE), 0),
    " SD =", round(sd(temp_summary_growth$length_fork, na.rm = TRUE), 0), "mm\n")
cat("  Detection span: mean =", round(mean(temp_summary_growth$n_years, na.rm = TRUE), 1),
    " SD =", round(sd(temp_summary_growth$n_years, na.rm = TRUE), 1), "years\n")

rm(list = ls(pattern = "^temp_summary_"))

cat("\n--- Analysis complete ---\n\n")



##### Optional: Export #####################################----
#-------------------------------------------------------------#
# IMPORTANT: File exports are commented out per FESL coding conventions.
# Uncomment when ready to save outputs.

# # Export per-fish growth table
# write.csv(data_fish_growth,
#           "03_outputs/data_fish_growth.csv", row.names = FALSE)

# # Export per-fish-year growth table
# write.csv(df_growth,
#           "03_outputs/df_growth.csv", row.names = FALSE)

# # Export growth trajectory plot
# ggplot2::ggsave("03_outputs/plot_growth_trajectory.png",
#                 plot   = plots$growth_trajectory,
#                 width  = 10, height = 6, units = "in", dpi = 300)



##### Cleanup ##############################################----
#-------------------------------------------------------------#
rm(list = ls(pattern = "^temp_"))
cat("Cleanup complete.\n")
