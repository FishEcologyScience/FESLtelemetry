## --------------------------------------------------------------#
## Functions: Von Bertalanffy Growth and Morphology Prediction
## Author: Paul Bzonek [Claude]
## Date Created: 2026-05-28
##
## Workflow mirrors 2026_Bzonek_Walleye_Spawning (Script1-2, Script2-3).
## Growth parameters drawn from params_df_VonBertalanffy package data.
## Morphology parameters drawn from params_df_morphology package data.
## --------------------------------------------------------------#

# Suppress R CMD check notes for package datasets referenced by bare name
utils::globalVariables(c("params_df_VonBertalanffy", "params_df_morphology"))




#-------------------------------------------------------------#
### calculate_age_at_tagging
#-------------------------------------------------------------#
#' @title Calculate Fish Age at Time of Tagging
#' @name calculate_age_at_tagging
#'
#' @description Calculates fish age at the time of acoustic tagging using the
#' von Bertalanffy growth equation, inverted to solve for age from fork length.
#' Adds \code{age_at_tag} (years) and \code{tag_year} (integer) columns to the
#' input data.
#'
#' @param data A per-fish data frame (one row per animal) containing fork length
#'   and tagging date. If an \code{atel} object is supplied, \code{data$animals}
#'   is used automatically.
#' @param species Character. Species common name matching a row in
#'   \code{params_df_VonBertalanffy} (e.g., \code{"Walleye"}). Not required if
#'   \code{vb_params} is supplied directly.
#' @param fork_length_col Character. Name of the column containing fork length
#'   measurements in mm. Default is \code{"length_fork"}.
#' @param tag_date_col Character. Name of the column containing tagging/release
#'   date (Date or POSIXct). Default is \code{"release_date"}.
#' @param vb_params Optional data frame overriding the built-in species lookup.
#'   Must contain numeric columns \code{linf}, \code{k}, and \code{t0}.
#'
#' @return Input data frame with two new columns:
#' \describe{
#'   \item{age_at_tag}{Estimated age (years) at time of tagging.}
#'   \item{tag_year}{Calendar year of tagging (integer).}
#' }
#'
#' @details
#' Equation: \eqn{age = -\ln(1 - FL / L_\infty) / K + t_0}
#'
#' Fork length is capped at \code{Linf - 0.01} before the log transformation to
#' prevent \code{log(0)} when a fish is at or near the asymptote.
#'
#' @seealso \code{\link{project_growth_forward}} for projecting age and fork
#'   length forward over detection years.
#'
#' @export
#'
calculate_age_at_tagging <- function(data,
                                      species        = NULL,
                                      fork_length_col = "length_fork",
                                      tag_date_col    = "release_date",
                                      vb_params       = NULL) {

  # Extract animals table from atel input
  #----------------------------#
  if (inherits(data, "atel")) {
    data <- as.data.frame(data$animals)
  }

  # Resolve VB parameters
  #----------------------------#
  if (is.null(vb_params)) {
    if (is.null(species)) {
      stop(
        "Provide either 'species' (matched to params_df_VonBertalanffy) or 'vb_params' directly.",
        call. = FALSE
      )
    }
    vb_params <- params_df_VonBertalanffy[params_df_VonBertalanffy$species_common == species, ]
    if (nrow(vb_params) == 0) {
      stop(
        "Species '", species, "' not found in params_df_VonBertalanffy.\n",
        "Available species: ", paste(params_df_VonBertalanffy$species_common, collapse = ", "),
        call. = FALSE
      )
    }
    vb_params <- vb_params[1, ]   # use first row if multiple populations exist
  }

  temp_linf <- vb_params$linf
  temp_k    <- vb_params$k
  temp_t0   <- vb_params$t0

  # Validate required columns
  #----------------------------#
  required_cols <- c(fork_length_col, tag_date_col)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ", paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Calculate age at tagging
  #----------------------------#
  temp_fl       <- as.numeric(data[[fork_length_col]])
  temp_tag_date <- data[[tag_date_col]]

  data$tag_year   <- as.integer(format(as.Date(temp_tag_date), "%Y"))
  # pmin caps FL just below Linf to prevent log(0)
  data$age_at_tag <- -log(1 - pmin(temp_fl, temp_linf - 0.01) / temp_linf) / temp_k + temp_t0

  rm(temp_linf, temp_k, temp_t0, temp_fl, temp_tag_date)

  data
}




#-------------------------------------------------------------#
### project_growth_forward
#-------------------------------------------------------------#
#' @title Project Fish Age and Fork Length Forward Over Detection Years
#' @name project_growth_forward
#'
#' @description Uses von Bertalanffy growth parameters to project each fish's
#' age and predicted fork length for each year it was detected post-tagging.
#' Adds \code{years_since_tag}, \code{age_pred}, and \code{predicted_FL} to a
#' per-fish-year data frame.
#'
#' @param data A per-fish-year data frame (e.g., a behaviour or residency summary
#'   with one row per animal-year). Must already contain age at tagging and
#'   tag year columns, typically added by \code{\link{calculate_age_at_tagging}}.
#' @param species Character. Species common name matching a row in
#'   \code{params_df_VonBertalanffy}. Not required if \code{vb_params} is supplied.
#' @param age_at_tag_col Character. Name of the age-at-tagging column.
#'   Default is \code{"age_at_tag"}.
#' @param tag_year_col Character. Name of the tagging year column.
#'   Default is \code{"tag_year"}.
#' @param detection_year_col Character. Name of the detection year column
#'   (integer or factor). Default is \code{"year"}. If the column is a factor,
#'   it is converted via \code{as.integer(as.character(x))} to avoid returning
#'   level indices.
#' @param fork_length_col Character. Name of the observed fork length column
#'   (at tagging). Used to prevent projected values from falling below the
#'   measured tagging length. Default is \code{"length_fork"}.
#' @param vb_params Optional data frame overriding the built-in species lookup.
#'   Must contain numeric columns \code{linf}, \code{k}, and \code{t0}.
#'
#' @return Input data frame with three new columns:
#' \describe{
#'   \item{years_since_tag}{Years elapsed from tagging to detection year.}
#'   \item{age_pred}{Projected age (years) at the detection year.}
#'   \item{predicted_FL}{Projected fork length (mm) at the detection year,
#'     rounded to 1 decimal. Cannot be less than the observed tagging length.}
#' }
#'
#' @details
#' Equations:
#' \itemize{
#'   \item \eqn{age_{pred} = age_{tag} + years\_since\_tag}
#'   \item \eqn{FL_{pred} = L_\infty \times (1 - e^{-K \times (age_{pred} - t_0)})}
#' }
#'
#' \code{pmax(predicted_FL, observed_FL)} prevents apparent shrinkage for fish
#' whose fork length at tagging already approaches \eqn{L_\infty}.
#'
#' @seealso \code{\link{calculate_age_at_tagging}} for the upstream step.
#'
#' @export
#'
project_growth_forward <- function(data,
                                    species             = NULL,
                                    age_at_tag_col      = "age_at_tag",
                                    tag_year_col        = "tag_year",
                                    detection_year_col  = "year",
                                    fork_length_col     = "length_fork",
                                    vb_params           = NULL) {

  # Resolve VB parameters
  #----------------------------#
  if (is.null(vb_params)) {
    if (is.null(species)) {
      stop(
        "Provide either 'species' (matched to params_df_VonBertalanffy) or 'vb_params' directly.",
        call. = FALSE
      )
    }
    vb_params <- params_df_VonBertalanffy[params_df_VonBertalanffy$species_common == species, ]
    if (nrow(vb_params) == 0) {
      stop(
        "Species '", species, "' not found in params_df_VonBertalanffy.\n",
        "Available species: ", paste(params_df_VonBertalanffy$species_common, collapse = ", "),
        call. = FALSE
      )
    }
    vb_params <- vb_params[1, ]
  }

  temp_linf <- vb_params$linf
  temp_k    <- vb_params$k
  temp_t0   <- vb_params$t0

  # Validate required columns
  #----------------------------#
  required_cols <- c(age_at_tag_col, tag_year_col, detection_year_col)
  missing_cols  <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ", paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Convert detection year column: factor -> integer via character to avoid level indices
  #----------------------------#
  temp_det_year <- data[[detection_year_col]]
  if (is.factor(temp_det_year)) {
    temp_det_year <- as.integer(as.character(temp_det_year))
  } else {
    temp_det_year <- as.integer(temp_det_year)
  }

  temp_age_at_tag <- as.numeric(data[[age_at_tag_col]])
  temp_tag_year   <- as.integer(data[[tag_year_col]])

  # Resolve observed fork length for pmax safety floor
  #----------------------------#
  if (fork_length_col %in% names(data)) {
    temp_fl_observed <- as.numeric(data[[fork_length_col]])
  } else {
    temp_fl_observed <- 0
  }

  # Project forward
  #----------------------------#
  data$years_since_tag <- temp_det_year - temp_tag_year
  data$age_pred        <- temp_age_at_tag + data$years_since_tag
  data$predicted_FL    <- temp_linf * (1 - exp(-temp_k * (data$age_pred - temp_t0)))
  # pmax prevents apparent shrinkage for fish near Linf
  data$predicted_FL    <- round(pmax(data$predicted_FL, temp_fl_observed), 1)

  rm(temp_linf, temp_k, temp_t0, temp_det_year, temp_age_at_tag,
     temp_tag_year, temp_fl_observed)

  data
}




#-------------------------------------------------------------#
### predict_morphology
#-------------------------------------------------------------#
#' @title Predict Fish Body Width or Mass from Fork Length
#' @name predict_morphology
#'
#' @description Predicts body width or mass from fork length using
#' species-specific regression parameters from the \code{params_df_morphology}
#' package dataset (Hamilton Harbour morphology study).
#'
#' @param fork_length Numeric vector. Fork length measurements in mm.
#' @param species Character. Species common name matching
#'   \code{params_df_morphology$species_common} (e.g., \code{"Walleye"}).
#' @param relationship Character. Which morphometric relationship to apply.
#'   One of:
#'   \describe{
#'     \item{\code{"width_fl"}}{Width ~ Fork Length (linear)}
#'     \item{\code{"mass_fl"}}{Mass ~ Fork Length (power law)}
#'     \item{\code{"mass_width"}}{Mass ~ Width (power law)}
#'     \item{\code{"log_width_fl"}}{log(Width) ~ log(FL) (log-linear, returns width in mm)}
#'     \item{\code{"width_mass"}}{Width ~ Mass (power law)}
#'   }
#' @param morph_params Optional data frame overriding the built-in lookup.
#'   Must contain numeric columns \code{intercept_a} and \code{slope_b}.
#'
#' @return Numeric vector of predicted values. Units depend on relationship:
#'   width relationships return mm; mass relationships return grams.
#'
#' @details
#' Linear relationships use: \eqn{y = a \times x + b}
#'
#' Power-law and log-linear relationships use: \eqn{y = a \times x^b}
#'
#' @seealso \code{\link{project_growth_forward}} for the upstream fork length
#'   projection step.
#'
#' @export
#'
predict_morphology <- function(fork_length,
                                species,
                                relationship = c("width_fl", "mass_fl", "mass_width",
                                                  "log_width_fl", "width_mass"),
                                morph_params = NULL) {

  relationship <- match.arg(relationship)

  # Resolve morphology parameters
  #----------------------------#
  if (is.null(morph_params)) {
    temp_row <- params_df_morphology[
      params_df_morphology$species_common == species &
      params_df_morphology$relationship   == relationship,
    ]
    if (nrow(temp_row) == 0) {
      stop(
        "No morphology parameters found for species '", species,
        "', relationship '", relationship, "'.\n",
        "Available species: ",
        paste(unique(params_df_morphology$species_common), collapse = ", "),
        call. = FALSE
      )
    }
    morph_params <- temp_row[1, ]
  }

  temp_a <- morph_params$intercept_a
  temp_b <- morph_params$slope_b

  # Apply equation
  #----------------------------#
  predicted <- if (relationship == "width_fl") {
    # Linear: Width = a * FL + b
    temp_a * as.numeric(fork_length) + temp_b
  } else {
    # Power law (all other relationships): y = a * x^b
    temp_a * as.numeric(fork_length)^temp_b
  }

  rm(temp_a, temp_b)

  predicted
}
