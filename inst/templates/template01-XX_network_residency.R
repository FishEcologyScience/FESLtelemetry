## --------------------------------------------------------------#
## Template: template01-XX_network_residency
## Title: Network Analysis and Station Residency
##
## Purpose:
##    Publication-level network analysis and station residency for acoustic
##    telemetry data. Produces movement network summaries, residency summaries,
##    and figure-ready plots following FESL standards.
##
##    Requires an atel object from positionRtools as input.
##    Run template00-XX_load_and_filter.R first.
##
## --------------------------------------------------------------#
## INPUTS:
##   atel_obj - positionRtools atel object (from template00)
##     Required atel detection columns:
##       - animal_id: Fish identifier
##       - detection_datetime_utc: Detection timestamp (POSIXct, UTC)
##       - receiver_sn: Receiver serial number
##       - deploy_lat: Receiver latitude (numeric)
##       - deploy_lon: Receiver longitude (numeric)
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
##     - positionRtools
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

# Initialize plots list
plots <- list()



##### Prepare detections from atel ################################----
#-------------------------------------------------------------#
cat("\n--- Preparing detections from atel object ---\n")

cat("  Detections:", format(nrow(atel_obj$detections), big.mark = ","), "\n")
cat("  Animals:", length(unique(atel_obj$detections$animal_id)), "\n")
cat("  Stations:", length(unique(atel_obj$detections$receiver_sn)), "\n")
cat("  Date range:",
    format(min(as.Date(atel_obj$detections$detection_datetime_utc, tz = "UTC")), "%Y-%m-%d"),
    "to",
    format(max(as.Date(atel_obj$detections$detection_datetime_utc, tz = "UTC")), "%Y-%m-%d"),
    "\n")



##### Station Residency ###########################################----
#-------------------------------------------------------------#
cat("\n--- Calculating station residency ---\n")

# param_residency_units: time units for residency output
param_residency_units <- "hours"   # options: "secs", "mins", "hours", "days"

# Pass atel_obj directly — column mapping handled automatically
df_residency <- FESLtelemetry::calculate_residency(
  data  = atel_obj,
  units = param_residency_units
)

cat("  Residency calculated for",
    length(unique(df_residency$animal_id)), "fish across",
    length(unique(df_residency$station_no)), "stations\n")
cat("  Mean daily residency:", round(mean(df_residency$residence, na.rm = TRUE), 2),
    param_residency_units, "\n")



##### Residency Heatmap ###########################################----
#-------------------------------------------------------------#
cat("\n--- Building residency heatmap ---\n")

# Station coordinate key (one row per station, using FESL-standard column names)
temp_station_key <- atel_obj$detections %>%
  dplyr::select(station_no = receiver_sn, deploy_lat, deploy_lon = deploy_lon) %>%
  dplyr::slice_head(by = station_no)

plots$residency_heatmap <- df_residency %>%
  dplyr::left_join(temp_station_key, by = "station_no") %>%
  dplyr::mutate(
    station_factor = forcats::fct_reorder(
      as.factor(station_no), deploy_lon, .fun = median
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



##### Network Analysis ############################################----
#-------------------------------------------------------------#
cat("\n--- Building movement network ---\n")

# Pass atel_obj directly — columns extracted automatically
network_data <- FESLtelemetry::network_summary(atel_obj)

cat("  Unique stations:", nrow(network_data$receiver.locations), "\n")
cat("  Unique movement pairs:",
    nrow(network_data$individual.moves), "\n")



##### Network Plot ################################################----
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



##### Summary Statistics ##########################################----
#-------------------------------------------------------------#
cat("\n--- SUMMARY STATISTICS ---\n")

cat("\n1. Dataset Overview\n")
cat("  Detections:", format(nrow(atel_obj$detections), big.mark = ","), "\n")
cat("  Animals:", length(unique(atel_obj$detections$animal_id)), "\n")
cat("  Stations:", length(unique(atel_obj$detections$receiver_sn)), "\n")
cat("  Date range:",
    format(min(as.Date(atel_obj$detections$detection_datetime_utc, tz = "UTC")), "%Y-%m-%d"),
    "to",
    format(max(as.Date(atel_obj$detections$detection_datetime_utc, tz = "UTC")), "%Y-%m-%d"),
    "\n")

cat("\n2. Residency Summary\n")
temp_summary_residency <- df_residency %>%
  dplyr::group_by(animal_id) %>%
  dplyr::summarize(
    total_residence  = sum(residence, na.rm = TRUE),
    n_stations       = dplyr::n_distinct(station_no),
    n_days           = dplyr::n_distinct(date),
    .groups          = "drop"
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



##### Optional: Export Data and Plots #############################----
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



##### Cleanup #####################################################----
#-------------------------------------------------------------#
rm(list = ls(pattern = "^temp_"))
cat("Cleanup complete.\n")
