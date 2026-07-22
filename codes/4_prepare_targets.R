#### CODE 3: prepare targets from CAPRI!

#### Author: Michael Wögerer
#### Date: 03/02/2025

# 1. Dynamically build the named vector mapping dictionary from crop_mapping
# We select unique pairs, drop any rows missing FAO or GLOBIOM info, and create a named vector
fao_to_globiom <- crop_mapping %>%
  distinct(FAO, GLOBIOM) %>%
  filter(!is.na(FAO) & !is.na(GLOBIOM)) %>%
  { setNames(.$GLOBIOM, .$FAO) }


#### add GLOBIOM targets
if(GLOBIOM_targets){
GLOB.targets <- readRDS("input/croptargets_GLOBIOM.rds")

GLOB.targets <- GLOB.targets %>% filter(substr(NUTS_ID,1,nchar(lama.reg))==lama.reg) %>% group_by(SPECIES) %>% summarise(BaseArea=sum(BaseArea), production=sum(production))
GLOB.targets.area <- GLOB.targets %>% ungroup() %>% mutate(value=BaseArea/100, GLOBIOM=SPECIES)%>% dplyr::select(GLOBIOM, value)
GLOB.targets.prod <- GLOB.targets %>% ungroup() %>% mutate(value=production/1000, GLOBIOM=SPECIES)%>% dplyr::select(GLOBIOM, value)

targets.area <- GLOB.targets.area
targets.prod <- GLOB.targets.prod
} else {

FAO_targets <- readRDS(file.path(target.path, "faostat_avg_1998_2002.rds"))



# BUGFIX namings in the FAO output
country_map <- c(
  "United Kingdom of Great Britain and Northern Ireland" = "Great-Britain",
  "Netherlands (Kingdom of the)"                         = "Netherlands"
)
matches <- FAO_targets$Area %in% names(country_map)
FAO_targets$Area[matches] <- country_map[FAO_targets$Area[matches]]





targets.area <- FAO_targets %>%
  filter(
    Area == country,
    Element == "Area harvested"
  ) %>%
  # Map names and apply the exact same area scaling factor
  mutate(
    GLOBIOM = fao_to_globiom[Item],
    value = Mean_Value / 100
  ) %>%
  # Remove any crops that didn't have a mapping or resulted in NaN
  filter(!is.na(GLOBIOM), !is.na(value)) %>%
  # Match the exact column format of targets.area
  dplyr::select(GLOBIOM, value)


# 2. Process FAO targets for PRODUCTION
targets.prod <- FAO_targets %>%
  filter(
    Area == country,
    Element == "Production"
  ) %>%
  # Map names and apply the exact same production scaling factor
  mutate(
    GLOBIOM = fao_to_globiom[Item],
    value = Mean_Value / 1000
  ) %>%
  # Handle missing mappings or NaN rows gracefully
  filter(!is.na(GLOBIOM), !is.na(value)) %>%
  # Match the exact column format of targets.prod
  dplyr::select(GLOBIOM, value)

}

### if yields not avialable make targets 0
which_zero <- final_yields %>% group_by(GLOBIOM_abr) %>% summarise(sum_YLD=sum(YLD, na.rm=TRUE)) %>% na.omit()
which_zero <- setdiff(targets.area$GLOBIOM, which_zero$GLOBIOM_abr)

targets.area$value[targets.area$GLOBIOM %in% which_zero] <- 0
targets.prod$value[targets.prod$GLOBIOM %in% which_zero] <- 0




##### add priors for positive targets and avialable yields
# Convert col_sums_temp to a named vector
col_sums <- as.numeric(col_sums_temp[1, ])
names(col_sums) <- colnames(col_sums_temp)

# Crops that have a non-zero target but less than 1% of their target area
to.correct <- targets.area %>%
  filter(value > 0) %>%
  filter(coalesce(col_sums[GLOBIOM], 0) < value / 100) %>%
  pull(GLOBIOM)



jjj <- "Barl"
#### prior check
for(jjj in to.correct){
  update.prior <- crop.area %>% left_join(final_yields %>% filter(GLOBIOM_abr==jjj) %>% group_by(CELLCODE) %>% summarise(YLD=mean(YLD, na.rm=T))) %>% mutate(new.pri=total_area*YLD) %>% dplyr::select(-NUTS_ID, -total_area, -YLD)
  colnames(update.prior)[2] <- jjj
  save.priors <- save.priors %>% dplyr::select(-any_of(jjj)) %>% left_join(update.prior)
}

if(length(to.correct)!=0){
  crop_cols <- setdiff(names(save.priors), "CELLCODE")

  # Row-wise sum of the crop columns
  save.priors[, total := rowSums(.SD), .SDcols = crop_cols]

  # Rescale each crop column by the row total
  save.priors[, (crop_cols) := lapply(.SD, function(x) x / total), .SDcols = crop_cols]

  # Remove the temporary 'total' column
  save.priors[, total := NULL]
}









AQUA_targets <- read.csv(file.path(target.path, "AQUASTAT_irri.csv"))



matches <- AQUA_targets$Area %in% names(country_map)
AQUA_targets$Area[matches] <- country_map[AQUA_targets$Area[matches]]

##### fix the mapping depending on availablity BEGINNING
# Items reported for this country
reported_items <- AQUA_targets %>%
  filter(Area == country, Value!=0) %>%
  mutate(Item = sub("Harvested irrigated temporary crop area: ", "", Variable)) %>%
  pull(Item)

# GLOBIOM crops already covered by reported individual items
covered_globiom <- aqua_to_globiom %>%
  filter(Item %in% reported_items) %>%
  pull(GLOBIOM)

# Mapping of aggregate groups
aggregate_map <- tibble(
  Item = c(
    rep("Other cereals", 7),
    rep("Leguminous crops", 3),
    rep("Other crops", 9)
  ),
  GLOBIOM = c(
    "Whea", "Barl", "Corn", "Mill", "Srgh", "Rice", "WRye",
    "Soya", "BeaD", "ChkP",
    "Pota", "Sgbt", "Sunf", "Rape", "Gnut",
    "Cott", "SugC", "Cass", "SwPo"
  )
)

# Add only crops that are not already covered individually
aqua_to_globiom_use <- bind_rows(
  aqua_to_globiom,
  aggregate_map %>%
    filter(!GLOBIOM %in% covered_globiom)
) %>%
  distinct(Item, GLOBIOM)

##### fix the mapping depending on availablity END

targets.irri <- AQUA_targets %>%
  filter(Area == country) %>%
  mutate(
    Item = sub("Harvested irrigated temporary crop area: ", "", Variable),
    value = Value * 10
  ) %>%
  left_join(aqua_to_globiom_use, by = "Item") %>%
  left_join(
    targets.area %>% rename(area.target = value),
    by = "GLOBIOM"
  ) %>%
  filter(!is.na(GLOBIOM), !is.na(value), !is.na(area.target), area.target > 0) %>%
  group_by(Item, Year) %>%
  mutate(
    share = case_when(
      sum(area.target, na.rm = TRUE) > 0 ~
        area.target / sum(area.target, na.rm = TRUE),
      TRUE ~
        1 / n()
    ),
    value = value * share
  ) %>%
  ungroup() %>%
  group_by(GLOBIOM) %>%
  summarise(irri.area = mean(value, na.rm = TRUE), .groups = "drop")


### targtet max 80% of total area
targets.irri <- targets.irri %>%
  left_join(
    targets.area %>% rename(area.target = value),
    by = "GLOBIOM"
  ) %>%
  mutate(
    irri.area = pmin(irri.area, 0.8 * area.target)
  ) %>%
  select(GLOBIOM, irri.area)


