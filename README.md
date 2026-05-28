# FESLtelemetry

**Publication-level acoustic telemetry analysis for the Fish Ecology Science Lab (FESL/DFO).**

FESLtelemetry is the analytical layer of the PositionR telemetry ecosystem. It accepts pre-processed data from positionRtools, provides station residency and movement network analysis, and standardizes lab workflows through a template system.

---

## Ecosystem Position

```text
positionRtools  →  Load / QC / Filter / Mortality / Summaries
                              ↓  (atel object)
glatos_exploreR →  Interactive exploration  →  [optional] filter_bookmark.json
                              ↓
FESLtelemetry   →  Residency / Network analysis / Publication figures / Lab templates
```

FESLtelemetry assumes data have already been QC-filtered and are provided as a positionRtools `atel` object. Plain GLATOS-format dataframes are also accepted for backwards compatibility.

---

## Installation

```r
# Install from GitHub
remotes::install_github("FishEcologyScience/FESL_package_telemetry")

# positionRtools is recommended (required for atel workflows)
remotes::install_github("jakebrownscombe/positionRtools")
```

---

## Quick Start

### Standard workflow (atel input)

```r
library(FESLtelemetry)

# Step 1: Get an atel object from positionRtools
# (run positionRtools QC + filtering first, then:)
atel_obj <- readRDS("my_processed_atel.rds")

# Step 2: Station residency
df_residency <- calculate_residency(atel_obj, units = "hours")

# Step 3: Movement network
network_data <- network_summary(atel_obj)
network_plot(network_data, shapefile = shapefile_HH_WGS84)
```

### Converting raw GLATOS files

```r
# Convert GLATOS files to atel in one call (requires positionRtools)
atel_obj <- raw_to_atel(
  detections  = "glatos_detections.rds",
  deployments = "glatos_receivers.rds",
  animals     = "glatos_animals.rds",
  source      = "GLATOS"
)
```

### Using the template system

```r
# Deploy the entry-point setup template
use_template("load_and_filter")

# Deploy the network + residency analysis template
use_template("network_residency")

# See all available templates
list_templates()
```

---

## Templates

Templates are standardized R scripts deployed to your project directory. Run them in order.

| Template key | File | Purpose |
| --- | --- | --- |
| `"load_and_filter"` | `template00-XX_load_and_filter.R` | Load atel object; optionally apply glatos_exploreR filter bookmark |
| `"network_residency"` | `template01-XX_network_residency.R` | Station residency + movement network analysis |
| `"summarize_dets"` | *(deprecated)* | Legacy GLATOS detection summary |

### Filter bookmark workflow

When a subset of data is identified interactively in glatos_exploreR, export a `filter_bookmark.json` from the app. template00 can apply those filters back to the full atel object:

```r
# In template00, uncomment the filter bookmark block:
param_bookmark_path <- "filter_bookmark.json"
temp_filters <- jsonlite::fromJSON(param_bookmark_path)
atel_obj <- positionRtools::filter_custom(
  atel_obj,
  animal_id %in% dplyr::filter(atel_obj$animals,
    species_common %in% temp_filters$filters$species)$animal_id
)
```

---

## Core Functions

### `calculate_residency(data, units = "hours", ...)`

Calculates fish residency times at acoustic receiver stations by tracking station-to-station transitions. Returns a dataframe with daily residency per fish per station.

Accepts either an `atel` object or a plain GLATOS-format dataframe.

```r
df_residency <- calculate_residency(atel_obj, units = "hours")
df_residency <- calculate_residency(glatos_df, station_col = "station_no")
```

### `network_summary(data, ...)`

Summarizes fish movement data into a network structure: receiver locations, pairwise movement matrix, individual movements, and plot-ready data. Accepts `atel` or plain dataframe.

```r
network_data <- network_summary(atel_obj)
```

### `network_plot(data, Min.traffic = 1, shapefile = NA, ...)`

Plots the movement network. Nodes are receivers (coloured by detection frequency), edges are movements (width scaled by frequency). Returns a ggplot object invisibly.

```r
p <- network_plot(network_data, shapefile = shapefile_HH_WGS84, labels = TRUE)
ggplot2::ggsave("network.png", p, width = 10, height = 8, dpi = 300)
```

### `raw_to_atel(detections, source = "GLATOS", ...)`

Thin wrapper around positionRtools loaders. Converts raw GLATOS, OTN, or Fathom files directly to an `atel` object. Requires positionRtools.

---

## Built-in Data

| Object | Description |
| --- | --- |
| `example_data_raw_dets` | 100,000 acoustic telemetry detections, Hamilton Harbour (GLATOS format) |
| `shapefile_HH_WGS84` | Hamilton Harbour shoreline boundary (sf, WGS84) |
| `basemap_HH` | Hamilton Harbour base map (ggmap raster) |

---

## Version History

| Version | Changes |
| --- | --- |
| 0.2.0 | atel input support for all core functions; `raw_to_atel()` conversion; template system redesigned around positionRtools ecosystem |
| 0.1.3 | Name change |
| 0.1.2 | Bug fixes, documentation |
| 0.1.1 | Initial release |
