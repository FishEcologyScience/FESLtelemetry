#' Calculate Fish Residency at Acoustic Receiver Stations
#'
#' @description Calculates fish residency times at acoustic receiver stations by
#' tracking movements between stations and computing time spent at each location.
#' This function identifies sequential detections at the same station and calculates
#' the duration between first and last detection in each residency event.
#'
#' @param data A dataframe containing acoustic telemetry detection data.
#' @param animal_col Character. Name of column containing fish/animal identifiers.
#' Default is "animal_id".
#' @param station_col Character. Name of column containing receiver station identifiers.
#' Default is "station_no".
#' @param timestamp_col Character. Name of column containing detection timestamps.
#' Default is "detection_timestamp_est".
#' @param date_col Character. Name of column containing detection dates.
#' Default is "date".
#' @param lat_col Character. Name of column containing receiver latitude coordinates.
#' Default is "deploy_lat".
#' @param long_col Character. Name of column containing receiver longitude coordinates.
#' Default is "deploy_long".
#' @param units Character. Time units for residency calculation. One of "hours" (default),
#' "mins", "days", or "secs". Passed to \code{difftime()}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Tracks movements by comparing current and previous detection locations
#'   \item Calculates time lag between consecutive detections
#'   \item Identifies unique movement events using a movement ID
#'   \item Computes residency duration for each station visit
#'   \item Summarizes total residency time by date, animal, and station
#' }
#'
#' Only consecutive detections of the same individual are considered for residency
#' calculations. Movements between different stations reset the residency counter.
#'
#' @return A dataframe with columns:
#' \describe{
#'   \item{date}{Detection date}
#'   \item{animal_id}{Fish identifier (or custom name from animal_col)}
#'   \item{station}{Station identifier (or custom name from station_col)}
#'   \item{residence}{Total residency time at station on that date (in specified units)}
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with default GLATOS column names
#' residency <- calculate_residency(data_det)
#'
#' # Custom column names
#' residency <- calculate_residency(
#'   data = my_data,
#'   animal_col = "fish_id",
#'   station_col = "receiver",
#'   timestamp_col = "timestamp"
#' )
#'
#' # Calculate residency in days instead of hours
#' residency <- calculate_residency(data_det, units = "days")
#' }
#'
#' @export
calculate_residency <- function(data,
                                animal_col = "animal_id",
                                station_col = "station_no",
                                timestamp_col = "detection_timestamp_est",
                                date_col = "date",
                                lat_col = "deploy_lat",
                                long_col = "deploy_long",
                                units = "hours") {

  # Normalize atel input to plain dataframe with FESL default column names
  #----------------------------#
  data <- .extract_atel_detections(data)

  # Validate inputs
  #----------------------------#
  required_cols <- c(animal_col, station_col, timestamp_col, date_col, lat_col, long_col)
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "\nAvailable columns: ",
      paste(names(data), collapse = ", ")
    )
  }

  # Validate units parameter
  valid_units <- c("secs", "mins", "hours", "days", "weeks")
  if (!units %in% valid_units) {
    stop(
      "Invalid units: '", units, "'\n",
      "Must be one of: ", paste(valid_units, collapse = ", ")
    )
  }

  # Track movements between stations
  #----------------------------#
  temp_movement <- data %>%
    # Select and rename columns using tidy evaluation
    # !!sym() converts character strings to column names
    # The !! "unquotes" the expression so dplyr can evaluate it
    select(
      timestamp = !!sym(timestamp_col),      # Rename user's timestamp column to 'timestamp'
      animal_id = !!sym(animal_col),         # Rename user's animal column to 'animal_id'
      station = !!sym(station_col),          # Rename user's station column to 'station'
      lat = !!sym(lat_col),                  # Rename user's lat column to 'lat'
      lon = !!sym(long_col),                 # Rename user's long column to 'lon'
      date = !!sym(date_col)                 # Rename user's date column to 'date'
    ) %>%
    arrange(animal_id, timestamp) %>%
    group_by(animal_id, date) %>%
    mutate(
      # Use lag() to get previous detection information for each fish
      from_station = lag(station),           # Station fish moved FROM
      from_animal_id = lag(animal_id),       # Previous animal_id (for validation)
      from_lat = lag(lat),                   # Previous latitude
      from_lon = lag(lon),                   # Previous longitude
      from_timestamp = lag(timestamp),       # Previous detection time
      lagtime = difftime(timestamp, from_timestamp, units = units), # Time since last detection
      # Create movement ID to identify unique residency events
      moveID = paste0(from_animal_id, from_station, station),
      moveID = cumsum(moveID != lag(moveID, default = first(moveID))) # Increment when movement changes
    ) %>%
    ungroup()

  # Filter and tally residence times
  #----------------------------#
  df_residency <- temp_movement %>%
    # Only keep detections where current and previous animal match (same fish)
    filter(animal_id == from_animal_id) %>%
    # Group by unique residency events
    group_by(date, animal_id, from_station, moveID) %>%
    summarize(
      detcount = n(),                        # Number of detections in this residency event
      time_start = min(from_timestamp),      # Start of residency
      time_end = max(timestamp),             # End of residency
      residence = difftime(time_end, time_start, units = units), # Duration at station
      .groups = "drop"
    ) %>%
    ungroup()

  # Sum residency by fish, date, and station
  #----------------------------#
  # A fish may have multiple residency events at the same station on the same day
  # Sum these to get total daily residency per station
  df_residency_summary <- df_residency %>%
    group_by(date, animal_id, station = from_station) %>%
    summarize(
      residence = as.numeric(sum(residence, na.rm = TRUE)), # Total residency time
      .groups = "drop"
    ) %>%
    ungroup()

  # Restore original column names in output
  #----------------------------#
  # The := operator is used for dynamic column naming in dplyr
  # !!animal_col := animal_id means "create a column with the name stored in animal_col"
  # This ensures the output uses the same column names as the input data
  df_residency_summary <- df_residency_summary %>%
    rename(
      !!animal_col := animal_id,             # Rename 'animal_id' back to user's original name
      !!station_col := station,              # Rename 'station' back to user's original name
      !!date_col := date                     # Rename 'date' back to user's original name
    )

  return(df_residency_summary)
}


# Internal helper: normalize atel object to FESL-standard plain dataframe
#
# Extracts the detections tibble from an atel object and renames columns to
# match calculate_residency() default parameter names. Plain dataframes pass
# through unchanged. Not exported.
.extract_atel_detections <- function(data) {

  if (!inherits(data, "atel")) return(data)

  dets <- as.data.frame(data$detections)

  # Rename atel column names to FESL defaults
  col_renames <- c(
    receiver_sn            = "station_no",
    detection_datetime_utc = "detection_timestamp_est",
    deploy_lon             = "deploy_long"
  )
  for (src in intersect(names(col_renames), names(dets))) {
    names(dets)[names(dets) == src] <- col_renames[[src]]
  }

  # Derive date column if not present
  if (!"date" %in% names(dets) && "detection_timestamp_est" %in% names(dets)) {
    dets$date <- as.Date(dets$detection_timestamp_est, tz = "UTC")
  }

  dets
}
