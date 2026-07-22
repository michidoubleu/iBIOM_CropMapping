##### CODE 1: prepare start areas from Linda map!

#### Author: Michael Wögerer
#### Date: 08/01/2025

## load 1km² country map
temp.EEA <- st_read(EEA.grid, quiet = TRUE)
temp.EEA <- temp.EEA %>% filter(CELLCODE%in%curr.EEA_NUTS$CELLCODE)

## load 100m LUM map
LUM.rast <- terra::rast(LUM)
LUM.rast <- crop(LUM.rast, ext(temp.EEA))

## load 100m LUM map
LUMenergy.rast <- terra::rast(LUMenergy)
LUMenergy.rast <- crop(LUMenergy.rast, ext(temp.EEA))

values(LUM.rast) <- values(LUMenergy.rast)
rm("LUMenergy.rast")

## overlay of 1km² country map and 100m LUM map
## Faster approach exploiting perfect alignment of grids

# extract raster values and cell coordinates
LUM.values <- terra::values(LUM.rast, mat = FALSE)
LUM.xy <- terra::xyFromCell(LUM.rast, seq_len(terra::ncell(LUM.rast)))

# assign each 100m cell to its 1km EEA cell
LUM.dt <- data.table(
  EOFORIGIN = floor(LUM.xy[, 1] / 1000) * 1000,
  NOFORIGIN = floor(LUM.xy[, 2] / 1000) * 1000,
  LUM.class = LUM.values
)

rm(LUM.values, LUM.xy)

# lookup table for EEA cells
EEA.lookup <- data.table::as.data.table(
  sf::st_drop_geometry(temp.EEA)
)[, .(CELLCODE, EOFORIGIN, NOFORIGIN)]

# assign CELLCODE to each 100m raster cell
LUM.dt <- LUM.dt[
  EEA.lookup,
  on = .(EOFORIGIN, NOFORIGIN),
  nomatch = 0
]

# remove missing raster values
LUM.dt <- LUM.dt[!is.na(LUM.class)]

# keep only crop classes
temp.LUM <- LUM.dt[LUM.class %in% lum_crop_map]

# aggregate area per 1km cell and LUM class
# each 100m cell = 0.01 km²
temp.LUM <- temp.LUM[
  ,
  .(total_area = .N * 0.01),
  by = .(CELLCODE, LUM.class)
]


crop.area <- temp.LUM[
  ,
  .(total_area = sum(total_area)),
  by = .(CELLCODE)
]
crop.area <- merge(crop.area, EEA_NUTS, by = "CELLCODE", all.x = TRUE)

unique.CELLCODES <- unique(crop.area$CELLCODE)
