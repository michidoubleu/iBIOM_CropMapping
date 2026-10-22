##### CODE 03: load inputs shared by all countries (once, before the country loop)

#### Author: Michael Wögerer

country_grid <- readRDS(country.grid.path)
country_grid <- country_grid %>% arrange(desc(years), lama.regs) %>% filter(years %in% RUN_YEARS)

EPIC_shp <- readRDS(EPIC.map.path)

coords <- sf::st_coordinates(EPIC_shp)

# EPIC points sit at the 1 km cell centres, but ~13% of them are off by up to 0.5 m
# (e.g. x mod 1000 = 499.73) -> round to the cell origin, otherwise the exact join in 07 drops them
EPIC_mapping_all <- data.table(
  SimUID = EPIC_shp$SimUID,
  EOFORIGIN = round((coords[,1] - 500) / 1000) * 1000,
  NOFORIGIN = round((coords[,2] - 500) / 1000) * 1000
)
EPIC_mapping_all <- unique(EPIC_mapping_all, by = c("EOFORIGIN", "NOFORIGIN"))

simu.shp.all <- read_sf(SimU.path)
simu.shp.all <- dplyr::select(simu.shp.all, simuID)
