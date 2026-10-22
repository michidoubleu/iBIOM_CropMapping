## load NUTS LUM
LAMA.NUTS <- read_sf("input/NUTS_LAMASUS/shp_nuts.shp")

yyy <- 2000
nnn <- "DE13"
full.mapping <- NULL

for(yyy in c("2000","2010","2018")){

## load 100m LUM map
LUM.rast <- terra::rast(paste0("./input/LUM_data/LUM_",yyy,".tif"))
LUMenergy.rast <- terra::rast(paste0("./input/FINAL_LAYERS_updated/LUM_",yyy,"_updated.tif"))


for(nnn in unique(LAMA.NUTS$NUTS_ID)){
  temp.NUTS <- LAMA.NUTS %>% filter(NUTS_ID == nnn)

  # wrap risky code in tryCatch
  res <- tryCatch({
    LUM.rast.temp <- crop(LUM.rast, ext(temp.NUTS))
    LUMenergy.rast.temp <- crop(LUMenergy.rast, ext(temp.NUTS))
    values(LUM.rast.temp) <- values(LUMenergy.rast.temp)
    rm("LUMenergy.rast.temp")

    ## overlay of 1km² country map and 100m LUM map
    temp.LUM <- exactextractr::exact_extract(LUM.rast.temp, temp.NUTS, force_df = TRUE)

    if(length(temp.LUM) == 0){
      return(NULL)
    }

    names(temp.LUM) <- nnn
    temp.LUM <- bind_rows(temp.LUM, .id = "NUTS_ID")
    temp.LUM <- as.data.table(temp.LUM)
    temp.LUM <- temp.LUM[, .(area = sum(coverage_fraction/100, na.rm = TRUE)),
                         by = .(NUTS_ID, value)]
    temp.LUM <- temp.LUM[!is.nan(value)]
    setnames(temp.LUM, old = "value", new = "LUM.class")
    as.data.frame(temp.LUM)

  }, error = function(e) {
    message(sprintf("Skipping %s: %s", nnn, e$message))
    return(NULL)  # skip this iteration
  })

  if(!is.null(res)){
    full.mapping <- full.mapping %>% bind_rows(res %>% mutate(year=yyy))
  }
}
}

save(full.mapping, file="4rupesh.RData")
load("4rupesh.RData")

write.csv(full.mapping,"LUM_NUTS.csv", row.names = F)

# mapping2 <- read.csv("./input/mappings/land_cover_mapping.csv")
# colnames(mapping2)[1] <- "LUM.class"
#
# mapping <- mapping2[,c(-2,-4)]
# table.full <- full.mapping %>% left_join(mapping) %>% dplyr::select(-LUM.class) %>% filter(substr(GLOBIOM_LC,1,3)=="LUM") %>% rename("LUM.class"="GLOBIOM_LC") %>% mutate(unit="km2") %>% group_by(NUTS_ID) %>% mutate(share=area/sum(area))
#
# table.full <- table.full[,c(1,3,2,4,5)]
# table.full <- table.full %>% ungroup() %>% arrange(NUTS_ID, LUM.class)
# write.csv(table.full, file="output/LUMcropNUTS.csv", row.names = F)
#
# mapping <- mapping2[,-2]
# table.full <- full.mapping %>% left_join(mapping) %>% dplyr::select(-LUM.class) %>% filter(substr(GLOBIOM_LC,1,3)=="LUM") %>% rename("GLOB.class"="GLOB2") %>% group_by(NUTS_ID, GLOB.class) %>% summarise(area=sum(area, na.rm=T)) %>% mutate(unit="km2") %>% group_by(NUTS_ID) %>% mutate(share=area/sum(area))
#
# table.full <- table.full[,c(1,3,2,4,5)]
# table.full <- table.full %>% ungroup() %>% arrange(NUTS_ID, GLOB.class)
# write.csv(table.full, file="output/GLOBIOMcropNUTS.csv", row.names = F)
