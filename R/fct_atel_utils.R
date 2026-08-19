#' Convert Raw Telemetry Files to atel Object
#'
#' @description A convenience wrapper around positionRtools loaders. Reads raw
#' detection (and optional deployment/animal) files in GLATOS, OTN, or Fathom
#' format and returns a standardised positionRtools \code{atel} object ready for
#' FESLtelemetry analysis templates.
#'
#' This function requires the \pkg{positionRtools} package. If you already have
#' an \code{atel} object from a positionRtools session, use it directly —
#' \code{raw_to_atel()} is only needed when starting from raw data files.
#'
#' @param detections Character. File path to the detection file (RDS or CSV).
#' @param deployments Character. File path to the deployment file. Optional;
#'   defaults to \code{NULL}.
#' @param animals Character. File path to the animal/tagging metadata file.
#'   Optional; defaults to \code{NULL}.
#' @param diagnostics Character. File path to the receiver diagnostics file.
#'   Optional; defaults to \code{NULL}.
#' @param source Character. Data format of the input files. One of
#'   \code{"GLATOS"} (default), \code{"OTN"}, or \code{"Fathom"}.
#' @param backend Character. Storage backend for the returned atel object.
#'   \code{"auto"} (default) selects tibble for small datasets and DuckDB for
#'   large ones. See \code{positionRtools::load_glatos()} for details.
#' @param ... Additional arguments passed to the positionRtools loader. Use
#'   this to supply format-specific parameters (e.g., \code{tag_specs} for OTN).
#'
#' @return An \code{atel} S3 object (positionRtools class) containing
#'   standardised detections, deployments, animals, and a processing log.
#'
#' @examples
#' \dontrun{
#' # From GLATOS files (most common FESL use case)
#' atel_obj <- raw_to_atel(
#'   detections  = "my_detections.rds",
#'   deployments = "my_deployments.rds",
#'   animals     = "my_animals.rds",
#'   source      = "GLATOS"
#' )
#'
#' # From OTN CSV exports
#' atel_obj <- raw_to_atel(
#'   detections  = "detections.csv",
#'   deployments = "deployments.xlsx",
#'   animals     = "animals.xlsx",
#'   source      = "OTN"
#' )
#' }
#'
#' @export
raw_to_atel <- function(detections,
                        deployments = NULL,
                        animals     = NULL,
                        diagnostics = NULL,
                        source      = c("GLATOS", "OTN", "Fathom"),
                        backend     = "auto",
                        ...) {

  if (!requireNamespace("positionRtools", quietly = TRUE)) {
    stop(
      "positionRtools is required for raw_to_atel().\n",
      "Install it with: remotes::install_github('jakebrownscombe/positionRtools')",
      call. = FALSE
    )
  }

  source <- match.arg(source)

  switch(source,
    GLATOS = positionRtools::load_glatos(
      detections  = detections,
      deployments = deployments,
      animals     = animals,
      diagnostics = diagnostics,
      backend     = backend,
      ...
    ),
    OTN = positionRtools::load_otn(
      detections  = detections,
      deployments = deployments,
      animals     = animals,
      diagnostics = diagnostics,
      backend     = backend,
      ...
    ),
    Fathom = positionRtools::load_fathom(
      detections  = detections,
      deployments = deployments,
      animals     = animals,
      backend     = backend,
      ...
    )
  )
}
