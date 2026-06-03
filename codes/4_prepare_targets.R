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

to.correct <- intersect(targets.area$GLOBIOM[targets.area$value!=0] , colnames(col_sums_temp)[col_sums_temp==0])

jjj <- "Rape"
#### prior check
for(jjj in to.correct){
  update.prior <- crop.area %>% dplyr::select(-NUTS_ID)
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