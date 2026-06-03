# Crop Distribution Downscaling System (CDDS)

## Overview
This repository contains the Crop Distribution Downscaling System (CDDS), an R-based pipeline designed to downscale crop area and production targets to a high-resolution 1 km² EEA reference grid. It utilizes land-use maps (LUM), crop suitability probabilities (Storm priors), and EPIC yield models to produce consistent local allocations. 

## Local Loop Execution Workflow
The main entry point for local execution is `main.R`. When `CLUSTER = FALSE` in your `user_config.R`, the pipeline iterates locally over the regions and years specified in `clustergrid` and processes each via a sequential chain of 7 scripts. 

Here is what each script does exactly, including its primary inputs and outputs:

### `0_initialize_routine.R`
**Purpose**: Sets up the environment for the current iteration (region and year). It determines file paths and loads required geographic boundaries and crop mappings.
- **Inputs**: Current `clustergrid` row variables, `user_config.R` paths, mapping CSVs (e.g., `CAPRI_GLOB_mapping.csv`, `EEAref_LAMAnuts_mapping_oct17.csv`).
- **Outputs**: Defined directory paths, `EEA_NUTS` region definitions, `crop_mapping`, and `curr.EEA_NUTS` configuration variables.

### `1_prepare_startmap_Linda_LUM.R`
**Purpose**: Prepares the starting agricultural areas using the 100m Land Use Map (LUM) overlaid on the 1km² EEA grid.
- **Inputs**: 1km² EEA country shapefiles (`EEA.grid`), 100m LUM raster, and LUM energy raster.
- **Outputs**: `crop.area` (total crop area per 1km² cell), `temp.LUM` (detailed LUM classes per cell), and `unique.CELLCODES`.

### `2_prepare_priors_Storm.R`
**Purpose**: Generates prior crop probabilities per grid cell based on Hugo Storm's multi-band suitability rasters. Converts bands to GLOBIOM crops and normalizes them into probability shares.
- **Inputs**: Prior raster, `crop.area`, `temp.EEA`, `crop_mapping`.
- **Outputs**: `save.priors` (matrix of probabilities for each crop per cell).

### `3_load_EPIC.R`
**Purpose**: Loads and joins spatial EPIC crop yield data. It maps EPIC simulation points to the EEA grid and gap-fills missing yield combinations with regional averages.
- **Inputs**: `EUEPIC_SimUID_1k_updt.rds`, `EPIC_yields.rds`, and GLOBIOM yield fallback (`cropyieldsOLD_GLOBIOM.rds`).
- **Outputs**: `final_yields` (table of crop yields per grid cell, scenario/management class, and GLOBIOM crop).

### `4_prepare_targets.R`
**Purpose**: Prepares the top-level area and production targets that the downscaling model must meet. The updated script dynamically toggles between **GLOBIOM** and historical **FAO (faostat_avg_1998_2002)** targets. It zeroes out targets lacking yield data and optionally assigns a fallback prior where necessary.
- **Inputs**: `croptargets_GLOBIOM.rds` or `faostat_avg_1998_2002.rds`, `final_yields`, `save.priors`.
- **Outputs**: `targets.area`, `targets.prod`, and an updated `save.priors` adjusted to cover positive targets.

### `5_area_matching.R`
**Purpose**: The core spatial allocation engine. It uses the `downscalr` package to distribute the regional `targets.area` into the 1km² `crop.area` using `save.priors` as weights. 
- **Inputs**: `crop.area`, `targets.area`, `save.priors`.
- **Outputs**: `result` (downscaled crop area allocations per cell), and an initial `final.alloc`.

### `6_production_matching.R`
**Purpose**: Refines the area allocation to also meet production targets. It achieves this by intelligently shifting crop areas between different management intensity levels (LUM classes `M1.rf` to `M5.rf`) where yields differ, running up to 5 iterative redistributions until production (`Area × Yield`) matches `targets.prod`.
- **Inputs**: `result` (from script 5), `temp.LUM`, `final_yields`, `targets.prod`.
- **Outputs**: `final.alloc` (the final adjusted crop distribution per cell across management classes) and `show.res` (a summary comparing the final allocated vs. targeted area and production).

---
After this sequence completes, `main.R` packages the outputs (`final.alloc`, `show.res`, `result`) and saves them as `.rds` files into the `output/res_v1/` directory for the specific region and year.
