##### CODE 1: prepare start areas from Linda map!

#### Author: Michael Wögerer
#### Date: 25/11/2024

library(dplyr)
library(tidyr)
library(sf)
library(terra)
library(exactextractr)

my.summary <- function(x){
  result <- x %>% group_by(value) %>% summarise(
    area = sum(coverage_fraction/100)
  )
  return(result)
}

folder.grid <- list.files("../geodata/EEA_1km/", full.names = TRUE)

args <- commandArgs(trailingOnly=TRUE)
JOB <- ifelse(.Platform$GUI == "RStudio",1,as.integer(args[[1]]))
dir.create("output")

cc <- folder.grid[JOB]


folder.files <- list.files(cc, full.names = TRUE)
folder.files <- folder.files[grep(".shp",folder.files)]

temp.agg <- st_read(folder.files, quiet = TRUE)


yy <- 2000
for (yy in c(2000,2010,2018)){
  temp.path <- paste0("../../../../3. Deliverables/2. WP2/Data/100 m LUM data sets/LUM_",yy,".tif")
  LUM.rast <- terra::rast(temp.path)
  LUM.rast <- crop(LUM.rast, ext(temp.agg))

  extraction <- exactextractr::exact_extract(LUM.rast, temp.agg, force_df=T)
  names(extraction) <- temp.agg$CELLCODE





}

