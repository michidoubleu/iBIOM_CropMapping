# Area mapping
area_map <- c(
  "Austria" = "Austria",
  "Belgium" = "Belgium",
  "Bulgaria" = "Bulgaria",
  "Croatia" = "Croatia",
  "Cyprus" = NA,                         # not in comp.table
  "Czechia" = "CzechRep",                        # not in comp.table
  "Denmark" = "Denmark",
  "Estonia" = "Estonia",
  "Finland" = "Finland",
  "France" = "France",
  "Germany" = "Germany",
  "Greece" = "Greece",
  "Hungary" = "Hungary",
  "Ireland" = "Ireland",
  "Italy" = "Italy",
  "Latvia" = "Latvia",
  "Lithuania" = "Lithuania",
  "Luxembourg" = NA,                     # not in comp.table
  "Malta" = NA,                          # not in comp.table
  "Netherlands (Kingdom of the)" = "Netherlands",
  "Poland" = "Poland",
  "Portugal" = "Portugal",
  "Romania" = "Romania",
  "Slovakia" = "Slovakia",
  "Slovenia" = "Slovenia",
  "Spain" = "Spain",
  "Sweden" = "Sweden"
)

# Item mapping
item_map <- c(
  "Barley" = "Barl",
  "Maize (corn)" = "Corn",
  "Potatoes" = "Pota",
  "Rape or colza seed" = "Rape",
  "Soya beans" = "Soya",
  "Sunflower seed" = "Sunf",
  "Wheat" = "Whea"
)


comp.table <- LUID.res %>% group_by(ANYREGION, SPECIES) %>% summarise(LUM.res=sum(value))

FAO <- read.csv("input/FAO_comparison/FAO_output.csv")
FAO <- FAO %>% dplyr::select(Area, Item, Value)


FAO_mapped <- FAO %>%
  mutate(
    ANYREGION = recode(Area, !!!area_map),
    SPECIES = recode(Item, !!!item_map)
  ) %>%
  filter(!is.na(ANYREGION), !is.na(SPECIES)) %>% dplyr::select(-Area,-Item)  # remove unmapped entries

comp.table %>% left_join(FAO_mapped %>% mutate(Value=Value
                                               w))
