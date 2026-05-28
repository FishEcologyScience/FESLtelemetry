## --------------------------------------------------------------#
## Script: create_vb_growth_params_xlsx.R
## Purpose: Create vb_growth_params.xlsx with a Parameters sheet and
##          an Instructions sheet explaining how to populate VB constants.
##          Run this script once to create the file. Add new species directly
##          to the Parameters sheet in Excel, then re-run format_growth_params.R.
## Author: Paul Bzonek [Claude]
## Date Created: 2026-05-28
## --------------------------------------------------------------#

library(openxlsx)

# ---- Parameters sheet data ---------------------------------#
params_data <- data.frame(
  species_common    = "Walleye",
  species_scientific = "Sander vitreus",
  linf              = 642.5,
  k                 = 0.375,
  t0                = -0.5,
  fl_units          = "mm",
  population        = "Hamilton Harbour - Lake Ontario",
  citation          = "Brooks et al. 2025",
  doi               = "https://doi.org/10.1186/s40462-024-00505-6",
  stringsAsFactors  = FALSE
)

# ---- Instructions sheet data --------------------------------#
instructions_data <- data.frame(
  Section = c(

    # ---- What is the VB equation ----
    "WHAT IS THE VON BERTALANFFY GROWTH EQUATION?",
    "",
    "The von Bertalanffy growth equation describes how fish length increases with age:",
    "   FL = Linf * (1 - exp(-K * (age - t0)))",
    "",
    "It can be rearranged to estimate age from fork length (used at tagging):",
    "   age = -log(1 - FL / Linf) / K + t0",
    "",

    # ---- Walleye example with numbers ----
    "EXAMPLE: Walleye (Hamilton Harbour, Brooks et al. 2025)",
    "",
    "Step 1 - Write the equation with the published parameter values:",
    "   FL = 642.5 * (1 - exp(-0.375 * (age - (-0.5))))",
    "",
    "Step 2 - Identify what each number represents:",
    "   642.5  =  Linf  =  asymptotic fork length (the maximum FL the fish approaches)",
    "   0.375  =  K     =  growth coefficient (how quickly the fish approaches Linf)",
    "  -0.5    =  t0    =  theoretical age at which fork length would equal zero",
    "",
    "Step 3 - Swap the numbers for column names to fill in the Parameters sheet:",
    "   linf  = 642.5",
    "   k     = 0.375",
    "   t0    = -0.5",
    "",

    # ---- Column definitions ----
    "COLUMN DEFINITIONS (Parameters sheet)",
    "",
    "species_common     Common name of the species (must match atel$animals$species_common)",
    "species_scientific Scientific name (genus species)",
    "linf               Asymptotic fork length in mm  (the Linf in the equation above)",
    "k                  Growth coefficient            (the K in the equation above)",
    "t0                 Theoretical age at FL = 0     (the t0 in the equation above)",
    "fl_units           Units for fork length and linf (use 'mm')",
    "population         Population or water body where the parameters were estimated",
    "citation           Short author-year citation (e.g., 'Brooks et al. 2025')",
    "doi                Full DOI URL for the source publication",
    "",

    # ---- How to add a species ----
    "HOW TO ADD A NEW SPECIES",
    "",
    "1. Find published von Bertalanffy parameters (Linf, K, t0) for your species",
    "   and population in the literature.",
    "2. Add a new row to the Parameters sheet with all columns filled in.",
    "3. Run data-raw/format_growth_params.R to regenerate the vb_growth_params",
    "   package data object.",
    "4. The new species will then be available in calculate_age_at_tagging() and",
    "   project_growth_forward() via the 'species' argument.",
    "",
    "NOTE: If multiple populations exist for the same species (e.g., two lakes),",
    "add one row per population. The functions will use the first matching row",
    "unless you supply vb_params directly."

  ),
  stringsAsFactors = FALSE
)

# ---- Build workbook ----------------------------------------#
wb <- openxlsx::createWorkbook()

# Styles
style_header <- openxlsx::createStyle(
  fontSize   = 11,
  fontColour = "#FFFFFF",
  fgFill     = "#2E5D8E",
  halign     = "LEFT",
  textDecoration = "bold"
)
style_section_heading <- openxlsx::createStyle(
  fontSize       = 10,
  textDecoration = "bold",
  fgFill         = "#D9E1F2"
)
style_equation <- openxlsx::createStyle(
  fontName = "Courier New",
  fontSize = 10,
  fgFill   = "#F2F2F2"
)
style_normal <- openxlsx::createStyle(
  fontSize = 10
)

# ---- Parameters sheet --------------------------------------#
openxlsx::addWorksheet(wb, "Parameters")

openxlsx::writeData(wb, "Parameters", params_data, startRow = 1, startCol = 1)

# Header row style
openxlsx::addStyle(
  wb, "Parameters",
  style = style_header,
  rows  = 1, cols = 1:ncol(params_data),
  gridExpand = TRUE
)

# Data rows style
openxlsx::addStyle(
  wb, "Parameters",
  style = style_normal,
  rows  = 2:(nrow(params_data) + 1), cols = 1:ncol(params_data),
  gridExpand = TRUE
)

# Column widths
openxlsx::setColWidths(
  wb, "Parameters",
  cols   = 1:ncol(params_data),
  widths = c(18, 22, 8, 8, 8, 10, 38, 24, 52)
)

# Freeze top row
openxlsx::freezePane(wb, "Parameters", firstRow = TRUE)

# ---- Instructions sheet ------------------------------------#
openxlsx::addWorksheet(wb, "Instructions")

openxlsx::writeData(
  wb, "Instructions",
  instructions_data,
  startRow = 1, startCol = 1,
  colNames = FALSE
)

# Style: section headings (rows starting with all-caps words)
temp_section_rows <- which(
  grepl("^[A-Z][A-Z ]+$", instructions_data$Section) &
  nchar(trimws(instructions_data$Section)) > 0
)
if (length(temp_section_rows) > 0) {
  openxlsx::addStyle(
    wb, "Instructions",
    style = style_section_heading,
    rows  = temp_section_rows, cols = 1,
    gridExpand = FALSE
  )
}

# Style: equation rows (rows with leading spaces + '=')
temp_equation_rows <- which(grepl("^   [A-Za-z_]+ ?[=*]", instructions_data$Section))
if (length(temp_equation_rows) > 0) {
  openxlsx::addStyle(
    wb, "Instructions",
    style = style_equation,
    rows  = temp_equation_rows, cols = 1,
    gridExpand = FALSE
  )
}

# Normal style for all others
openxlsx::addStyle(
  wb, "Instructions",
  style = style_normal,
  rows  = 1:nrow(instructions_data), cols = 1,
  gridExpand = FALSE
)
# Re-apply section and equation styles on top (addStyle overwrites)
if (length(temp_section_rows) > 0) {
  openxlsx::addStyle(
    wb, "Instructions",
    style = style_section_heading,
    rows  = temp_section_rows, cols = 1
  )
}
if (length(temp_equation_rows) > 0) {
  openxlsx::addStyle(
    wb, "Instructions",
    style = style_equation,
    rows  = temp_equation_rows, cols = 1
  )
}

openxlsx::setColWidths(wb, "Instructions", cols = 1, widths = 85)

rm(temp_section_rows, temp_equation_rows)

# ---- Save --------------------------------------------------#
openxlsx::saveWorkbook(
  wb,
  file      = "data-raw/vb_growth_params.xlsx",
  overwrite = TRUE
)

cat("vb_growth_params.xlsx created.\n")
cat("  Sheets: Parameters, Instructions\n")
cat("  Species in Parameters:", nrow(params_data), "\n")
