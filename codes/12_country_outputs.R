##### CODE 12: national output files (area, yield and additional variables as gpkg, parquet, COG)

library(sf)
library(dplyr)
library(tidyr)
library(terra)

# ── 1. Format and Pivot Spatial Attributes ────────────────────────────────────

# Create unique LUM-crop layer names
cntr_res <- final.alloc %>%
  mutate(layer_name = paste0(lu.to, "_", LUM.class))

# Pivot Area data to wide (CELLCODE × layer columns)
area_wide <- cntr_res %>%
  dplyr::select(CELLCODE, layer_name, value) %>%
  pivot_wider(
    id_cols = CELLCODE,
    names_from = layer_name,
    values_from = value,
    values_fill = 0,
    values_fn = sum
  ) %>%
  rename_with(~ paste0(.), -CELLCODE) # Suffix "_A" for Area

# Pivot per-ha attributes (yield + additional variables) to wide (CELLCODE × layer columns)
# file label -> column in final.alloc
attr_layers <- c(yield = "YLD", setNames(extra_vars, extra_vars))
attr_layers <- attr_layers[attr_layers %in% names(cntr_res)]

attr_wide <- lapply(attr_layers, function(v) {
  cntr_res %>%
    filter(lu.to != "OthAgr") %>%
    dplyr::select(CELLCODE, layer_name, all_of(v)) %>%
    pivot_wider(
      id_cols = CELLCODE,
      names_from = layer_name,
      values_from = all_of(v),
      values_fill = NA_real_,
      values_fn = mean
    )
})


# ── 2. Join to Spatial Vector ──────────────────────────────────────────────────

# Attach the data attributes flawlessly based on the shared CELLCODE string
# temp.EEA is already an 'sf' object for the current country
area_sf <- temp.EEA %>%
  left_join(area_wide, by = "CELLCODE") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
attr_sf <- lapply(attr_wide, function(w) {
  temp.EEA %>%
    left_join(w, by = "CELLCODE") %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
})

# Ensure the output directory exists
dir.create(paste0("output/", RESULT_TAG, "/country_res/"), recursive = TRUE, showWarnings = FALSE)

# Grab the current country name (assuming 'lama.reg' holds the loop's country string)
current_country <- lama.reg


# ── 3. Export as GeoPackage (.gpkg) — preserves long column names ─────────────

area_gpkg_path <- sprintf("output/%s/country_res/%s_area_%s_results.gpkg", RESULT_TAG, RESULT_TAG, current_country)
st_write(area_sf, area_gpkg_path, delete_dsn = TRUE, quiet = TRUE)

for (lbl in names(attr_sf)) {
  gpkg_path <- sprintf("output/%s/country_res/%s_%s_%s_results.gpkg", RESULT_TAG, RESULT_TAG, lbl, current_country)
  st_write(attr_sf[[lbl]], gpkg_path, delete_dsn = TRUE, quiet = TRUE)
}


# ── 4. Export as GeoParquet (.parquet) ─────────────────────────────────────────
# Requires GDAL >=3.5 built with Arrow/Parquet support.
# If this errors with "Parquet driver not found", install/enable that GDAL build,
# or swap these two calls for: sfarrow::st_write_parquet(area_sf, area_parquet_path)

if ("Parquet" %in% st_drivers()$name) {
  area_parquet_path <- sprintf("output/%s/country_res/%s_area_%s_results.parquet", RESULT_TAG, RESULT_TAG, current_country)
  st_write(area_sf, area_parquet_path, driver = "Parquet", delete_dsn = TRUE, quiet = TRUE)

  for (lbl in names(attr_sf)) {
    parquet_path <- sprintf("output/%s/country_res/%s_%s_%s_results.parquet", RESULT_TAG, RESULT_TAG, lbl, current_country)
    st_write(attr_sf[[lbl]], parquet_path, driver = "Parquet", delete_dsn = TRUE, quiet = TRUE)
  }
} else {
  message("  GDAL Parquet driver not available, skipping .parquet export")
}


# ── 5. Export as Cloud-Optimized GeoTIFF (stacked, one band per layer) ────────
# Rasterizes each numeric attribute column onto a common grid derived from the
# CELLCODE polygons themselves, then stacks all bands into one COG per file.
# Assumes temp.EEA is a regular grid of square cells (true for EEA reference grids).
library(sf)
library(dplyr)
library(terra)

# ── Fast grid rasterizer: for polygon layers that are a perfect regular grid ──
fast_rasterize_grid <- function(poly_sf, out_path, id_col = "CELLCODE") {

  value_cols <- poly_sf %>%
    st_drop_geometry() %>%
    dplyr::select(where(is.numeric), -any_of(id_col)) %>%
    names()

  if (length(value_cols) == 0) {
    stop("No numeric columns found to rasterize in ", out_path)
  }

  # Centroids give exact grid coordinates (fast, no overlay needed)
  cent <- st_centroid(poly_sf) %>% st_coordinates()

  # Infer resolution from spacing between unique centroid coordinates
  ux <- sort(unique(round(cent[, 1], 1)))
  uy <- sort(unique(round(cent[, 2], 1)))
  res_x <- min(diff(ux))
  res_y <- min(diff(uy))
  if (abs(res_x - res_y) > 1e-6) {
    warning("x/y resolution differ (", res_x, " vs ", res_y, ") — grid may not be square")
  }
  res <- res_x

  # Grid extent (cell edges, not centroids)
  xmin <- min(ux) - res / 2
  xmax <- max(ux) + res / 2
  ymin <- min(uy) - res / 2
  ymax <- max(uy) + res / 2
  ncols <- round((xmax - xmin) / res)
  nrows <- round((ymax - ymin) / res)

  # Integer row/col index for every polygon, straight from its centroid
  col_idx <- floor((cent[, 1] - xmin) / res) + 1
  row_idx <- nrows - floor((cent[, 2] - ymin) / res)  # flip: raster row 1 = top (ymax)

  template <- rast(
    nrows = nrows, ncols = ncols,
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    crs = st_crs(poly_sf)$wkt
  )
  cell_no <- cellFromRowCol(template, row_idx, col_idx)

  df <- st_drop_geometry(poly_sf)

  # Direct vectorized assignment per band — no polygon overlay
  band_list <- lapply(value_cols, function(col) {
    r <- template
    vals <- rep(NA_real_, ncell(template))
    vals[cell_no] <- df[[col]]
    values(r) <- vals
    r
  })
  r_stack <- rast(band_list)
  names(r_stack) <- value_cols

  writeRaster(
    r_stack,
    out_path,
    filetype = "COG",
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "OVERVIEWS=AUTO"),
    overwrite = TRUE
  )

  invisible(r_stack)
}

area_cog_path <- sprintf("output/%s/country_res/%s_area_%s_results.tif", RESULT_TAG, RESULT_TAG, current_country)
fast_rasterize_grid(area_sf, area_cog_path)

for (lbl in names(attr_sf)) {
  cog_path <- sprintf("output/%s/country_res/%s_%s_%s_results.tif", RESULT_TAG, RESULT_TAG, lbl, current_country)
  fast_rasterize_grid(attr_sf[[lbl]], cog_path)
}