##### SUPPORT: EPIC 1 km point map (SimUID) from shapefile to rds — reading the shapefile is slow

#### Author: Michael Wögerer

EPIC_shp <- read_sf(file.path(INPUT_DIR, "EPIC/MAP/EUEPIC_SimUID_1k_updt.shp"))

saveRDS(EPIC_shp, file = EPIC.map.path)
