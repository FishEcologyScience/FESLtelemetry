## --------------------------------------------------------------#
## Template: template01-XX_network_residency
## Title: Network Analysis and Station Residency
##
## Purpose:
##    Publication-level network analysis and station residency for acoustic
##    telemetry data. Produces movement network summaries, residency summaries,
##    and figure-ready plots following FESL standards.
##
##    Two input paths are provided — choose one and comment out the other.
##
## --------------------------------------------------------------#
## INPUTS:
##   PATH A (atel, recommended):
##     atel_obj  - positionRtools atel object (from template00)
##
##   PATH B (raw GLATOS dataframe):
##     df_raw    - GLATOS-format detection dataframe
##       Required columns: animal_id, detection_timestamp_est,
##         station_no, deploy_lat, deploy_long, date
##
## OUTPUTS:
##   df_residency       - Residency times by date, animal, and station
##   network_data       - Network summary list (from network_summary())
##   plots              - List of figure-ready ggplot objects:
##     $network           - Movement network map
##     $residency_heatmap - Station residency heatmap by date
##
## DEPENDENCIES:
##   Required packages:
##     - tidyverse (dplyr, ggplot2, forcats)
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

# Select input path: "atel" or "glatos"
param_input_path <- "atel"

# Initialize plots list
plots <- list()



##### Prepare detections ###################################----
#-------------------------------------------------------------#
cat("\n--- Preparing detections ---\n")

## PATH A: atel input (recommended) ----------------------------#
if (param_input_path == "atel") {

  cat("  Input: atel object\n")
  cat("  Detections:", format(nrow(atel_obj$detections), big.mark = ","), "\n")
  cat("  Animals:", length(unique(atel_obj$detections$animal_id)), "\n")
  cat("  Stations:", length(unique(atel_obj$detections$receiver_sn)), "\n")
  cat("  Date range:",
      format(min(as.Date(atel_obj$detections$detection_datetime_utc, tz = "UTC")), "%Y-%m-%d"),
      "to",
      format(max(as.Date(atel_obj$detections$detection_datetime_utc, tz = "UTC")), "%Y-%m-%d"),
      "\n")

}

## PATH B: raw GLATOS dataframe --------------------------------#
# if (param_input_path == "glatos") {
#
#   cat("  Input: raw GLATOS dataframe\n")
#   cat("  Detections:", format(nrow(df_raw), big.mark = ","), "\n")
#   cat("  Animals:", length(unique(df_raw$animal_id)), "\n")
#   cat("  Stations:", length(unique(df_raw$station_no)), "\n")
#   cat("  Date range:",
#       format(min(df_raw$date), "%Y-%m-%d"),
#       "to",
#       format(max(df_raw$date), "%Y-%m-%d"),
#       "\n")
#
# }



##### Station Residency ########################################----
#-------------------------------------------------------------#
cat("\n--- Calculating station residency ---\n")

# param_residency_units: time units for residency output
param_residency_units <- "hours"   # options: "secs", "mins", "hours", "days"

## PATH A: atel input ------------------------------------------#
if (param_input_path == "atel") {

  # Pass atel_obj directly — column mapping handled automatically
  df_residency <- FESLtelemetry::calculate_residency(
    data  = atel_obj,
    units = param_residency_units
  )

}

## PATH B: raw GLATOS dataframe --------------------------------#
# if (param_input_path == "glatos") {
#
#   df_residency <- FESLtelemetry::calculate_residency(
#     data          = df_raw,
#     animal_col    = "animal_id",
#     station_col   = "station_no",
#     timestamp_col = "detection_timestamp_est",
#     date_col      = "date",
#     lat_col       = "deploy_lat",
#     long_col      = "deploy_long",
#     units         = param_residency_units
#   )
#
# }

cat("  Residency calculated for",
    length(unique(df_residency$animal_id)), "fish across",
    length(unique(df_residency$receiver_sn)), "stations\n")
cat("  Mean daily residency:", round(mean(df_residency$residence, na.rm = TRUE), 2),
    param_residency_units, "\n")



##### Residency Heatmap ########################################----
#-------------------------------------------------------------#
cat("\n--- Building residency heatmap ---\n")

## PATH A: station coordinate key from atel -------------------#
if (param_input_path == "atel") {

  temp_station_key <- atel_obj$detections %>%
    dplyr::select(receiver_sn, deploy_lat, deploy_lon) %>%
    dplyr::slice_head(by = receiver_sn)

}

## PATH B: station coordinate key from GLATOS df --------------#
# if (param_input_path == "glatos") {
#
#   temp_station_key <- df_raw %>%
#     dplyr::select(receiver_sn = station_no, deploy_lat, deploy_lon = deploy_long) %>%
#     dplyr::slice_head(by = receiver_sn)
#
# }

plots$residency_heatmap <- df_residency %>%
  dplyr::left_join(temp_station_key, by = "receiver_sn") %>%
  dplyr::mutate(
    station_factor = forcats::fct_reorder(
      as.factor(receiver_sn), deploy_lon, .fun = median
    )
  ) %>%
  ggplot2::ggplot(aes(x = date, y = station_factor, fill = residence)) +
  geom_tile() +
  viridis::scale_fill_viridis_c(end = 0.85, na.value = "grey90") +
  labs(
    x    = "Date",
    y    = "Station (ordered west to east)",
    fill = paste0("Daily residency\n(", param_residency_units, ")")
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("  Residency heatmap created\n")

rm(list = ls(pattern = "^temp_"))



##### Network Analysis #########################################----
#-------------------------------------------------------------#
cat("\n--- Building movement network ---\n")

## PATH A: atel input ------------------------------------------#
if (param_input_path == "atel") {

  # Pass atel_obj directly — columns extracted automatically
  network_data <- FESLtelemetry::network_summary(atel_obj)

}

## PATH B: raw GLATOS dataframe --------------------------------#
# if (param_input_path == "glatos") {
#
#   network_data <- FESLtelemetry::network_summary(
#     data       = df_raw,
#     FishID     = df_raw$animal_id,
#     ReceiverID = df_raw$station_no,
#     lat        = df_raw$deploy_lat,
#     long       = df_raw$deploy_long
#   )
#
# }

cat("  Unique stations:", nrow(network_data$receiver.locations), "\n")
cat("  Unique movement pairs:", nrow(network_data$individual.moves), "\n")



##### Network Plot #############################################----
#-------------------------------------------------------------#
cat("\n--- Plotting movement network ---\n")

# param_min_traffic: minimum movements to display an edge
param_min_traffic <- 1

# network_plot() prints to device and returns the ggplot object invisibly
plots$network <- FESLtelemetry::network_plot(
  data        = network_data,
  Min.traffic = param_min_traffic,
  labels      = FALSE
  # shapefile = your_sf_object   # uncomment to overlay a boundary shapefile
  # e.g., shapefile = FESLtelemetry::shapefile_HH_WGS84
)

cat("  Network plot created\n")



##### Summary Statistics #######################################----
#-------------------------------------------------------------#
cat("\n--- SUMMARY STATISTICS ---\n")

cat("\n1. Dataset Overview\n")
if (param_input_path == "atel") {
  cat("  Detections:", format(nrow(atel_obj$detections), big.mark = ","), "\n")
  cat("  Animals:", length(unique(atel_obj$detections$animal_id)), "\n")
  cat("  Stations:", length(unique(atel_obj$detections$receiver_sn)), "\n")
  cat("  Date range:",
      format(min(as.Date(atel_obj$detections$detection_datetime_utc, tz = "UTC")), "%Y-%m-%d"),
      "to",
      format(max(as.Date(atel_obj$detections$detection_datetime_utc, tz = "UTC")), "%Y-%m-%d"),
      "\n")
}

cat("\n2. Residency Summary\n")
temp_summary_residency <- df_residency %>%
  dplyr::group_by(animal_id) %>%
  dplyr::summarize(
    total_residence = sum(residence, na.rm = TRUE),
    n_stations      = dplyr::n_distinct(receiver_sn),
    n_days          = dplyr::n_distinct(date),
    .groups         = "drop"
  )
cat("  Mean total residency per fish:",
    round(mean(temp_summary_residency$total_residence), 2),
    "±", round(sd(temp_summary_residency$total_residence), 2),
    param_residency_units, "\n")
cat("  Mean stations visited per fish:",
    round(mean(temp_summary_residency$n_stations), 1),
    "±", round(sd(temp_summary_residency$n_stations), 1), "\n")

cat("\n3. Network Summary\n")
cat("  Movement pairs detected:", nrow(network_data$individual.moves), "\n")
cat("  Unique receiver-to-receiver links:", nrow(network_data$plot.data), "\n")

rm(list = ls(pattern = "^temp_summary_"))

cat("\n--- Analysis complete ---\n\n")



##### Optional: Export Data and Plots ##########################----
#-------------------------------------------------------------#
# IMPORTANT: File exports are commented out per FESL coding conventions.
# Uncomment when ready to save outputs.

# # Export residency table
# write.csv(df_residency,
#           "03_outputs/df_residency.csv", row.names = FALSE)

# # Export network summary data
# saveRDS(network_data,
#         "03_outputs/network_data.rds")

# # Export plots
# ggplot2::ggsave("03_outputs/plot_residency_heatmap.png",
#                 plot   = plots$residency_heatmap,
#                 width  = 12, height = 6, units = "in", dpi = 300)

# # Export network plot
# ggplot2::ggsave("03_outputs/plot_network.png",
#                 plot   = plots$network,
#                 width  = 10, height = 8, units = "in", dpi = 300)



##### Cleanup ##################################################----
#-------------------------------------------------------------#
rm(list = ls(pattern = "^temp_"))
cat("Cleanup complete.\n")
