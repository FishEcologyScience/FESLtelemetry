#' Calculate Fish Residency at Acoustic Receiver Stations
#'
#' @description Calculates fish residency times at acoustic receiver stations by
#' tracking movements between stations and computing time spent at each location.
#' This function identifies sequential detections at the same station and calculates
#' the duration between first and last detection in each residency event.
#'
#' @param data A dataframe containing acoustic telemetry detection data.
#'   Alternatively, a positionRtools \code{atel} object — in which case column
#'   mapping is handled automatically from the detections component.
#' @param animal_col Character. Name of column containing fish/animal identifiers.
#'   Default is \code{"animal_id"}.
#' @param station_col Character. Name of column containing receiver station identifiers.
#'   Default is \code{"receiver_sn"} (atel standard). For plain GLATOS dataframes,
#'   pass \code{station_col = "station_no"} or let the automatic FESL-to-atel
#'   column mapping handle it.
#' @param timestamp_col Character. Name of column containing detection timestamps.
#'   Default is \code{"detection_datetime_utc"} (atel standard).
#' @param date_col Character. Name of column containing detection dates.
#'   Default is \code{"date"}.
#' @param lat_col Character. Name of column containing receiver latitude coordinates.
#'   Default is \code{"deploy_lat"}.
#' @param long_col Character. Name of column containing receiver longitude coordinates.
#'   Default is \code{"deploy_lon"} (atel standard).
#' @param units Character. Time units for residency calculation. One of \code{"hours"}
#'   (default), \code{"mins"}, \code{"days"}, or \code{"secs"}. Passed to
#'   \code{difftime()}.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Normalizes input to atel column naming standard (plain FESL/GLATOS
#'     dataframes have legacy column names mapped up automatically)
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
#'   \item{animal_id}{Fish identifier (or custom name from \code{animal_col})}
#'   \item{receiver_sn}{Station identifier (or custom name from \code{station_col})}
#'   \item{residence}{Total residency time at station on that date (in specified units)}
#' }
#'
#' @examples
#' \dontrun{
#' # With atel input (recommended)
#' df_residency <- calculate_residency(atel_obj, units = "hours")
#'
#' # With plain GLATOS dataframe (FESL column names mapped automatically)
#' df_residency <- calculate_residency(
#'   data          = example_data_raw_dets,
#'   animal_col    = "animal_id",
#'   station_col   = "station_no",
#'   timestamp_col = "detection_timestamp_est",
#'   date_col      = "date",
#'   lat_col       = "deploy_lat",
#'   long_col      = "deploy_long",
#'   units         = "hours"
#' )
#' }
#'
#' @export
calculate_residency <- function(data,
                                animal_col    = "animal_id",
                                station_col   = "receiver_sn",
                                timestamp_col = "detection_datetime_utc",
                                date_col      = "date",
                                lat_col       = "deploy_lat",
                                long_col      = "deploy_lon",
                                units         = "hours") {

  # Normalize input: atel extraction or FESL-to-atel column rename
  #----------------------------#
  data <- .normalize_to_atel_names(data)

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
    select(
      timestamp = !!sym(timestamp_col),
      animal_id = !!sym(animal_col),
      station   = !!sym(station_col),
      lat       = !!sym(lat_col),
      lon       = !!sym(long_col),
      date      = !!sym(date_col)
    ) %>%
    arrange(animal_id, timestamp) %>%
    group_by(animal_id, date) %>%
    mutate(
      from_station   = lag(station),
      from_animal_id = lag(animal_id),
      from_lat       = lag(lat),
      from_lon       = lag(lon),
      from_timestamp = lag(timestamp),
      lagtime        = difftime(timestamp, from_timestamp, units = units),
      moveID         = paste0(from_animal_id, from_station, station),
      moveID         = cumsum(moveID != lag(moveID, default = first(moveID)))
    ) %>%
    ungroup()

  # Filter and tally residence times
  #----------------------------#
  df_residency <- temp_movement %>%
    filter(animal_id == from_animal_id) %>%
    group_by(date, animal_id, from_station, moveID) %>%
    summarize(
      detcount   = n(),
      time_start = min(from_timestamp),
      time_end   = max(timestamp),
      residence  = difftime(time_end, time_start, units = units),
      .groups    = "drop"
    ) %>%
    ungroup()

  # Sum residency by fish, date, and station
  #----------------------------#
  df_residency_summary <- df_residency %>%
    group_by(date, animal_id, station = from_station) %>%
    summarize(
      residence = as.numeric(sum(residence, na.rm = TRUE)),
      .groups   = "drop"
    ) %>%
    ungroup()

  # Restore caller-specified column names in output
  #----------------------------#
  df_residency_summary <- df_residency_summary %>%
    rename(
      !!animal_col  := animal_id,
      !!station_col := station,
      !!date_col    := date
    )

  return(df_residency_summary)
}


# Internal helper: normalize input to atel column naming standard.
#
# For atel objects: extracts data$detections (already uses atel names).
# For plain dataframes: renames FESL legacy column names to atel standard
# (station_no -> receiver_sn, detection_timestamp_est -> detection_datetime_utc,
# deploy_long -> deploy_lon). Derives 'date' from detection_datetime_utc if absent.
# Not exported.
.normalize_to_atel_names <- function(data) {

  if (inherits(data, "atel")) {
    dets <- as.data.frame(data$detections)
    if (!"date" %in% names(dets) && "detection_datetime_utc" %in% names(dets)) {
      dets$date <- as.Date(dets$detection_datetime_utc, tz = "UTC")
    }
    return(dets)
  }

  # Plain dataframe: map FESL legacy names up to atel standard
  col_renames <- c(
    station_no              = "receiver_sn",
    detection_timestamp_est = "detection_datetime_utc",
    deploy_long             = "deploy_lon"
  )
  for (src in intersect(names(col_renames), names(data))) {
    names(data)[names(data) == src] <- col_renames[[src]]
  }

  # Derive date if absent
  if (!"date" %in% names(data) && "detection_datetime_utc" %in% names(data)) {
    data$date <- as.Date(data$detection_datetime_utc, tz = "UTC")
  }

  data
}
