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
temp.LUM <- exactextractr::exact_extract(LUM.rast, temp.EEA, force_df=T)
names(temp.LUM) <- temp.EEA$CELLCODE
temp.LUM <- data.table::rbindlist(temp.LUM, idcol = "CELLCODE")
temp.LUM <- temp.LUM[, .(area = sum(coverage_fraction/100, na.rm = TRUE)), by = .(CELLCODE, value)]
temp.LUM <- temp.LUM[!is.nan(value)]
setnames(temp.LUM, old = "value", new = "LUM.class")


temp.LUM <- temp.LUM[LUM.class %in% lum_crop_map]
temp.LUM <- temp.LUM[, .(total_area = sum(area)), by = .(CELLCODE, LUM.class)]

crop.area <- temp.LUM[, .(total_area = sum(total_area)), by = .(CELLCODE)]
crop.area_old <- merge(crop.area, EEA_NUTS, by = "CELLCODE", all.x = TRUE)

unique.CELLCODES <- unique(crop.area$CELLCODE)
