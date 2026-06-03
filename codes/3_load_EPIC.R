##### CODE 5: EPIC matiching

#### Author: Michael Wögerer
#### Date: 03/02/2025
source(file.path(INPUT_DIR, "mappings/FAO_eurostat_EPIC_GLOBIOM_crop_mapping.R"))

# # Read the shapefile (slow, but only once)
# EPIC_shp <- read_sf("./input/EPIC/MAP/EUEPIC_SimUID_1k_updt.shp")
#
# # Save it as an RDS file
# saveRDS(EPIC_shp, file = "./input/EPIC/MAP/EUEPIC_SimUID_1k_updt.rds")

EPIC_shp <- readRDS(file.path(INPUT_DIR, "EPIC/MAP/EUEPIC_SimUID_1k_updt.rds"))
EPIC_shp <- st_crop(EPIC_shp,ext(temp.EEA))

# Ensure CRS is the same
temp.EEA <- st_transform(temp.EEA, st_crs(EPIC_shp))
# Perform spatial join (assign CELLCODE to each point in EPIC_shp)
EPIC_mapping <- st_join(EPIC_shp, temp.EEA, left = FALSE) %>%
  select(CELLCODE, SimUID) %>% st_drop_geometry()


YLDG_data <- readRDS(file.path(INPUT_DIR, "EPIC/EPIC_yields.rds"))

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


##### Hopefully temporal fix: Add yield for crops outside of new EPIC runs
### Load GLOBIOM crop data
final.cropdat.old <- readRDS(file.path(INPUT_DIR, "EPIC/cropyieldsOLD_GLOBIOM.rds"))
### add SimU to cellcode mapping
simu.shp <- read_sf(SimU.path)
simu.shp <- st_crop(simu.shp,ext(temp.EEA))
simu.shp <- simu.shp %>% dplyr::select(-uniqueID)


simu_mapping <- st_join(simu.shp, temp.EEA, left = FALSE) %>%
  select(CELLCODE, simuID) %>% st_drop_geometry() %>% rename("SimUID"="simuID")


final.cropdat.old <- left_join(simu_mapping %>% mutate(SimUID=as.character(SimUID)),final.cropdat.old) %>% dplyr::select(-SimUID) %>% filter(!GLOBIOM_abr%in%unique(final_yields$GLOBIOM_abr))


final_yields <- final_yields %>% bind_rows(final.cropdat.old) %>% na.omit()




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
