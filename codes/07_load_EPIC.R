##### CODE 07: EPIC matching — yields and additional variables per cell x management x crop

#### Author: Michael Wögerer
#### Date: 03/02/2025
source(file.path(INPUT_DIR, "mappings/FAO_eurostat_EPIC_GLOBIOM_crop_mapping.R"))

eurostat_GLOBIOM_mapping$GLOBIOM_abr[
  eurostat_GLOBIOM_mapping$GLOBIOM_abr == "SugB"
] <- "Sgbt"

eurostat_GLOBIOM_mapping$GLOBIOM_abr[
  eurostat_GLOBIOM_mapping$GLOBIOM_abr == "Rye"
] <- "WRye"



##### faster replacement
EEA_lookup <- as.data.table(
  sf::st_drop_geometry(temp.EEA)
)[, .(CELLCODE, EOFORIGIN, NOFORIGIN)]

EPIC_mapping <- EPIC_mapping_all[
  EEA_lookup,
  on = .(EOFORIGIN, NOFORIGIN),
  nomatch = 0
][, .(CELLCODE, SimUID)]

#### END replacement


YLDG_data <- readRDS(EPIC.yields.path)

YLDG_data <- YLDG_data %>% filter(SimUID %in% unique(EPIC_mapping$SimUID))


final_yields <- EPIC_mapping %>% full_join(YLDG_data %>% dplyr::select(SimUID, Scen, CROP, YLD))




# Step 1: Identify missing CELLCODE/SimUID rows
missing_rows <- final_yields %>%
  filter(is.na(Scen) & is.na(CROP))

# Step 2: Compute average YLD for each Scen and CROP over all available data
avg_yields <- final_yields %>%
  filter(!is.na(Scen) & !is.na(CROP) & !is.na(YLD)) %>%
  group_by(Scen, CROP) %>%
  summarise(avg_YLD = mean(YLD, na.rm = TRUE), .groups = "drop")

# Step 3: Create a full combination of missing CELLCODE/SimUID with all Scens and Crops
expanded_rows <- missing_rows %>%
  select(CELLCODE, SimUID) %>%
  distinct() %>%
  crossing(avg_yields)  # Generates all combinations

# Step 4: Bind back to original dataset
final_yields <- final_yields %>%
  filter(!is.na(Scen) & !is.na(CROP)) %>%  # Keep original non-missing rows
  bind_rows(expanded_rows %>%
              rename(YLD = avg_YLD))       # Add filled-in rows

final_yields <- final_yields %>% left_join(eurostat_GLOBIOM_mapping %>% dplyr::select(EPIC_abr, GLOBIOM_abr) %>% rename("CROP"="EPIC_abr")) %>% dplyr::select(CELLCODE, Scen, GLOBIOM_abr, YLD)


##### replace old code as faster than
final_yields <- as.data.table(final_yields)
final_yields <- final_yields[
  ,
  .(YLD = mean(YLD, na.rm = TRUE)),
  by = .(CELLCODE, Scen, GLOBIOM_abr)
]
final_yields <- as_tibble(final_yields)



##### Hopefully temporal fix: Add yield for crops outside of new EPIC runs
### Load GLOBIOM crop data
final.cropdat.old <- readRDS(GLOBIOM.yields.path)

### add SimU to cellcode mapping
simu.shp <- st_crop(simu.shp.all, ext(temp.EEA))
st_crs(simu.shp) <- st_crs(temp.EEA)


simu_mapping <- st_join(simu.shp, temp.EEA, left = FALSE) %>%
  select(CELLCODE, simuID) %>% st_drop_geometry() %>% rename("SimUID"="simuID")

### (the former Croatia-only fix that dropped EPIC crops is no longer needed since the
###  EPIC point offset is corrected in 03_load_shared_inputs.R — see README, section 5)

### added fix to filter out crops where new yields are available
new.yield.avail <- unique(final_yields$GLOBIOM_abr)
final.cropdat.old <- final.cropdat.old %>% filter(!GLOBIOM_abr%in%new.yield.avail)


final.cropdat.old <- left_join(simu_mapping %>% mutate(SimUID=as.character(SimUID)),final.cropdat.old) %>% dplyr::select(-SimUID) %>% filter(!GLOBIOM_abr%in%unique(final_yields$GLOBIOM_abr))

# Ensure distinct rows per CELLCODE, Scen (LUM class), and GLOBIOM_abr by averaging duplicate yields
final.cropdat.old <- final.cropdat.old %>%
  group_by(CELLCODE, Scen, GLOBIOM_abr) %>%
  summarise(YLD = mean(YLD, na.rm = TRUE), .groups = "drop")


final_yields <- final_yields %>% bind_rows(final.cropdat.old) %>% na.omit()

##### bugfix, need all yields:
# 1. First, ensure columns are in your strict sequential order
ordered_cols <- c("CELLCODE", "GLOBIOM_abr", "M1.rf", "M2.rf", "M3.rf", "M4.rf", "M5.rf", "M5.ir")

final_yields <- final_yields %>%
  # 1. Pivot and order
  pivot_wider(names_from = Scen, values_from = YLD) %>%
  select(all_of(ordered_cols)) %>%

  # 2. Vectorized Forward-Fill (Left to Right)
  # It takes the current column's value, or falls back to the previous filled column
  mutate(
    F1 = M1.rf,
    F2 = coalesce(M2.rf, F1),
    F3 = coalesce(M3.rf, F2),
    F4 = coalesce(M4.rf, F3),
    F5 = coalesce(M5.rf, F4),
    F6 = coalesce(M5.ir, F5)
  ) %>%

  # 3. Vectorized Backward-Fill (Right to Left)
  # This catches any NAs that were at the very beginning of a row
  mutate(
    M5.ir = F6,
    M5.rf = coalesce(F5, M5.ir),
    M4.rf = coalesce(F4, M5.rf),
    M3.rf = coalesce(F3, M4.rf),
    M2.rf = coalesce(F2, M3.rf),
    M1.rf = coalesce(F1, M2.rf)
  ) %>%

  # 4. Clean up temporary columns
  select(-F1, -F2, -F3, -F4, -F5, -F6) %>%

  # 5. Pivot back to the original long format
  pivot_longer(
    cols = all_of(ordered_cols[-c(1,2)]),
    names_to = "Scen",
    values_to = "YLD"
  )


##### Additional management-dependent variables (fertilizer, irrigation water, SOC, C flux)
##### Same sources as the yields: EPIC for crops with EPIC yields, old GLOBIOM otherwise.
##### Crops without own EPIC data take crosswalk_vars from the most similar EPIC crop (crosswalk).
##### Remaining gaps are filled with the regional crop x management average.
vars.long <- data.table(CELLCODE = character(), Scen = character(), GLOBIOM_abr = character())
EPIC_cell <- NULL

if (file.exists(EPIC.vars.path)) {
  EPIC_cell <- readRDS(EPIC.vars.path) %>% filter(SimUID %in% unique(EPIC_mapping$SimUID))
  EPIC_cell <- EPIC_mapping %>%
    inner_join(EPIC_cell, by = "SimUID") %>%
    left_join(eurostat_GLOBIOM_mapping %>% dplyr::select(EPIC_abr, GLOBIOM_abr) %>% rename("CROP"="EPIC_abr"), by = "CROP") %>%
    filter(GLOBIOM_abr %in% new.yield.avail) %>%
    dplyr::select(CELLCODE, Scen, GLOBIOM_abr, any_of(extra_vars)) %>%
    as.data.table()
  vars.long <- rbindlist(list(vars.long, EPIC_cell), fill = TRUE)
}

if (file.exists(GLOBIOM.vars.path)) {
  GLOB_vars <- readRDS(GLOBIOM.vars.path) %>% filter(!GLOBIOM_abr %in% new.yield.avail)
  GLOB_vars <- simu_mapping %>% mutate(SimUID = as.character(SimUID)) %>%
    inner_join(GLOB_vars %>% mutate(SimUID = as.character(SimUID)), by = "SimUID") %>%
    dplyr::select(CELLCODE, Scen, GLOBIOM_abr, any_of(extra_vars))
  vars.long <- rbindlist(list(vars.long, GLOB_vars), fill = TRUE)
}

for (v in setdiff(extra_vars, names(vars.long))) vars.long[, (v) := NA_real_]

# a cell can overlap several SimUs -> average
vars.long <- vars.long[, lapply(.SD, mean, na.rm = TRUE), by = .(CELLCODE, Scen, GLOBIOM_abr), .SDcols = extra_vars]
for (v in extra_vars) set(vars.long, which(is.nan(vars.long[[v]])), v, NA_real_)

# crosswalk: same cell and management class, values of the proxy crop replace the crop's own (GLOBIOM) values
crosswalk <- read.csv(crosswalk.path) %>%
  filter(!GLOBIOM_abr %in% new.yield.avail, EPIC_proxy %in% new.yield.avail)
xw.vars <- intersect(crosswalk_vars, extra_vars)

if (!is.null(EPIC_cell) && nrow(crosswalk) > 0 && length(xw.vars) > 0) {
  xw <- EPIC_cell[GLOBIOM_abr %in% crosswalk$EPIC_proxy,
                  lapply(.SD, mean, na.rm = TRUE), by = .(CELLCODE, Scen, EPIC_proxy = GLOBIOM_abr), .SDcols = xw.vars]
  xw <- merge(xw, as.data.table(crosswalk)[, .(EPIC_proxy, GLOBIOM_abr)], by = "EPIC_proxy", allow.cartesian = TRUE)
  xw[, EPIC_proxy := NULL]
  setnames(xw, xw.vars, paste0(xw.vars, ".xw"))

  vars.long <- merge(vars.long, xw, by = c("CELLCODE", "Scen", "GLOBIOM_abr"), all = TRUE)
  for (v in xw.vars) {
    vars.long[GLOBIOM_abr %in% crosswalk$GLOBIOM_abr, (v) := get(paste0(v, ".xw"))]
    vars.long[, (paste0(v, ".xw")) := NULL]
  }
  message("  Crosswalk (", paste(xw.vars, collapse = ", "), "): ",
          paste(crosswalk$GLOBIOM_abr, crosswalk$EPIC_proxy, sep = "<-", collapse = ", "))
}

final_yields <- final_yields %>%
  left_join(as_tibble(vars.long), by = c("CELLCODE", "Scen", "GLOBIOM_abr")) %>%
  group_by(GLOBIOM_abr, Scen) %>%
  mutate(across(all_of(extra_vars), ~ coalesce(.x, mean(.x, na.rm = TRUE)))) %>%
  ungroup() %>%
  mutate(across(all_of(extra_vars), ~ ifelse(is.nan(.x), NA_real_, .x)))

missing.vars <- final_yields %>% group_by(GLOBIOM_abr) %>%
  summarise(across(all_of(extra_vars), ~ all(is.na(.x)))) %>%
  tidyr::pivot_longer(-GLOBIOM_abr) %>% filter(value)
if (nrow(missing.vars) > 0) {
  message("  No data for: ", paste(missing.vars$GLOBIOM_abr, missing.vars$name, sep = ":", collapse = ", "))
}


#### bugfix if yield is 0 then PRIOR 0
#  update priors where yield is 0 or missing
max_yld_per_cell <- final_yields %>%
  group_by(CELLCODE, GLOBIOM_abr) %>%
  summarise(max_YLD = max(YLD, na.rm = TRUE), .groups = "drop") %>%
  as.data.table()

priors_long <- data.table::melt(save.priors, id.vars = "CELLCODE", variable.name = "GLOBIOM_abr", value.name = "prior")
priors_long$GLOBIOM_abr <- as.character(priors_long$GLOBIOM_abr)

# Join with yields. Note: OthAgr will not match any yield.
priors_long <- merge(priors_long, max_yld_per_cell, by = c("CELLCODE", "GLOBIOM_abr"), all.x = TRUE)

# If max_YLD is 0, or if it's NA (missing from final_yields) AND the crop is not OthAgr, set prior to 0
priors_long[GLOBIOM_abr != "OthAgr" & (max_YLD == 0 | is.na(max_YLD)), prior := 0]

# Reshape back to wide
save.priors <- data.table::dcast(priors_long, CELLCODE ~ GLOBIOM_abr, value.var = "prior")

# Re-normalize priors to sum to 1
numeric_cols <- names(save.priors)[sapply(save.priors, is.numeric)]
save.priors[, row_sum := rowSums(.SD), .SDcols = numeric_cols]
save.priors[row_sum == 0, OthAgr := 1] # If everything was 0, dump residual into OthAgr
save.priors[row_sum == 0, row_sum := 1]
save.priors[, (numeric_cols) := lapply(.SD, function(x) x / row_sum), .SDcols = numeric_cols]
save.priors[, row_sum := NULL]
