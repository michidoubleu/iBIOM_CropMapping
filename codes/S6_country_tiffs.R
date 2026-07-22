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

# Pivot Yield data to wide (CELLCODE × layer columns)
yield_wide <- cntr_res %>%
  dplyr::select(CELLCODE, layer_name, YLD) %>%
  pivot_wider(
    id_cols = CELLCODE,
    names_from = layer_name,
    values_from = YLD,
    values_fill = NA_real_,
    values_fn = mean
  ) %>%
  rename_with(~ paste0(.), -CELLCODE) # Suffix "_Y" for Yield


# ── 2. Join to Spatial Vector and Export ──────────────────────────────────────

# Attach the data attributes flawlessly based on the shared CELLCODE string
# temp.EEA is already an 'sf' object for the current country
area_sf <- temp.EEA %>%
  left_join(area_wide, by = "CELLCODE") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
yld_sf <- temp.EEA %>%
  left_join(area_wide, by = "CELLCODE")%>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Ensure the output directory exists
dir.create(paste0("output/", RESULT_TAG, "/country_res/"), recursive = TRUE, showWarnings = FALSE)

# Grab the current country name (assuming 'lama.reg' holds the loop's country string)
current_country <- lama.reg

# Export as GeoPackage (.gpkg) - STRONGLY RECOMMENDED to preserve long column names
area_path <- sprintf("output/%s/country_res/%s_area_%s_results.gpkg", RESULT_TAG, RESULT_TAG, current_country)
st_write(area_sf, area_path, delete_dsn = TRUE, quiet = TRUE)

# Export as GeoPackage (.gpkg) - STRONGLY RECOMMENDED to preserve long column names
yield_path <- sprintf("output/%s/country_res/%s_yield_%s_results.gpkg", RESULT_TAG, RESULT_TAG, current_country)
st_write(yld_sf, yield_path, delete_dsn = TRUE, quiet = TRUE)
