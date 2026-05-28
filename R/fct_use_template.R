#' Use Analysis Template
#'
#' @description Copies a template analysis script to your project directory.
#' Templates provide standardized workflows that combine package functions with
#' customizable analysis code following FESL naming conventions.
#'
#' @param template Character. Name of the template to use. Available templates:
#' \itemize{
#'   \item \code{"load_and_filter"} - Load positionRtools atel object; optionally
#'     apply glatos_exploreR filter bookmark (template00 — run first)
#'   \item \code{"network_residency"} - Movement network analysis and station
#'     residency from atel input (template01)
#'   \item \code{"summarize_dets"} - (Deprecated) Detection data summary using
#'     raw GLATOS-format dataframes. Use \code{"load_and_filter"} for new workflows.
#' }
#' @param output_dir Character. Directory where the template script will be saved.
#' Default is "02_scripts" following project_template conventions.
#' @param overwrite Logical. Should existing files be overwritten? Default is FALSE.
#'
#' @return Invisibly returns the path to the created file.
#'
#' @examples
#' \dontrun{
#' # Recommended: start every analysis session with template00
#' use_template("load_and_filter")
#'
#' # Create in custom directory
#' use_template("load_and_filter", output_dir = "03_scripts")
#' }
#'
#' @export
use_template <- function(template, output_dir = "02_scripts", overwrite = FALSE) {

  # Validate template name
  available_templates <- c("load_and_filter", "network_residency", "summarize_dets")

  if (!template %in% available_templates) {
    stop(
      "Template '", template, "' not found.\n",
      "Available templates: ", paste(available_templates, collapse = ", ")
    )
  }

  # Warn on deprecated template
  if (template == "summarize_dets") {
    warning(
      "'summarize_dets' is deprecated. ",
      "Use 'load_and_filter' for atel-based workflows.",
      call. = FALSE
    )
  }

  # Map template names to file names
  template_files <- c(
    load_and_filter  = "template00-XX_load_and_filter.R",
    network_residency = "template01-XX_network_residency.R",
    summarize_dets   = "template01-XX_summarize_dets.R"
  )

  template_file <- template_files[[template]]

  # Get template path from package installation
  template_path <- system.file(
    "templates",
    template_file,
    package = "FESLtelemetry"
  )

  if (template_path == "") {
    stop("Template file not found in package installation.")
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created directory: ", output_dir)
  }

  # Determine output path
  output_path <- file.path(output_dir, template_file)

  # Check if file exists
  if (file.exists(output_path) && !overwrite) {
    stop(
      "File already exists: ", output_path, "\n",
      "Set overwrite = TRUE to replace it."
    )
  }

  # Copy template to output directory
  file.copy(template_path, output_path, overwrite = overwrite)

  message("Template created: ", output_path)
  message("\nNext steps:")
  message("1. Open the template script: ", template_file)
  message("2. Customize the script for your analysis")
  message("3. Update the Author and Date fields in the header")

  invisible(output_path)
}


#' List Available Templates
#'
#' @description Shows all available analysis templates in the FESLtelemetry package.
#'
#' @return A character vector of available template names.
#'
#' @examples
#' \dontrun{
#' list_templates()
#' }
#'
#' @export
list_templates <- function() {
  templates <- c(
    "load_and_filter"  = "Load atel object + optional glatos_exploreR filter bookmark (template00 — run first)",
    "network_residency" = "Movement network analysis and station residency from atel input (template01)",
    "summarize_dets"   = "[DEPRECATED] Detection summary using raw GLATOS dataframes"
  )

  cat("Available FESLtelemetry templates:\n\n")
  for (i in seq_along(templates)) {
    cat("  ", names(templates)[i], "\n")
    cat("    ", templates[i], "\n\n")
  }

  invisible(names(templates))
}
