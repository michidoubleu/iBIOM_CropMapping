library(gdxrrw)
library(dplyr)
library(tidyr)
library(ggplot2)
igdx("C:/GAMS/49")

source("codes/S3_read_gms.R")

crop_file <- list.files("input/GLOBIOM/",pattern = "*.gdx", full.names = TRUE)
crop.dat <- gdxrrw::rgdx.param(file.path(crop_file), symName = "CROP_DATA")

# recode all factor columns to characters in crop.dat
crop.dat.proc <- crop.dat %>% mutate(across(where(is.factor), as.character)) %>% filter(SPECIES==ALLITEM|ALLITEM=="BaseArea", nchar(ALLTECH)==2) %>% mutate(ALLITEM=ifelse(ALLITEM=="BaseArea","BaseArea","Yield")) %>% pivot_wider(names_from=ALLITEM, values_from=CROP_DATA, values_fill = 0) %>% mutate(BaseArea=BaseArea*1000)

# add simu mapping... it is from LU, so not uniquely identified. have to check how to account to NUTS regions
## maybe overlay this grid with capri grid or divide area uniformly per simu and map to LU-Soil-Slope-Elev
### going with the second option, seems quicker and more straight forward
#### if ever needs to be adjusted could add weighting of arable land per simu to account production to those areas

# LUID_map <- read_gms_data("input/mappings/SimUIDlu_map.gms",header.line = 31, data.line = c(33,425446))
# LUID_map <- LUID_map %>% filter(!grepl("_Any", AltiClass)) %>% rename("AllColRow"="ALLCOLROW", "ANYREGION"="ALLCOUNTRY")

full.map <- readRDS("input/SimU_CR_LU_map.rds")
full.map <- full.map %>% rename("AllColRow"="ALLCOLROW", "ANYREGION"="ALLCOUNTRY", "AEZCLASS"="AezClass") %>% dplyr::select(-ColRow30)

crop.dat.proc <- crop.dat.proc %>% left_join(full.map)

check <- crop.dat.proc %>% group_by(ANYREGION, AllColRow, AltiClass, SlpClass, SoilClass, AEZCLASS, SPECIES, ALLTECH) %>% summarise(duplication.fact=n())
### calc area and prod per simu
crop.dat.proc <- crop.dat.proc %>% left_join(check) %>% mutate(BaseArea=BaseArea/duplication.fact, production=Yield*BaseArea)
final.cropdat <- crop.dat.proc %>% group_by(SimUID, SPECIES) %>% summarise(BaseArea=sum(BaseArea), production=sum(production))



### map to CAPRI regions for crop processing
# capri.targets <- read.csv("input/CAPRI_CAPREG/preprocessed_CAPREG.csv", row.names = 1)
reg.map <- readRDS(file="./input/mappings/SimU_NUTS_weighted.rds") #### BUG, this needs to be updated as other mapping

reg.map <- reg.map %>% mutate(NUTS_ID=ifelse(substr(NUTS_ID,1,2)=="BG","BG",NUTS_ID))

crop.targets.GLOB <- final.cropdat %>% mutate(SimUID=as.numeric(SimUID)) %>% left_join(reg.map) %>% na.omit() %>% mutate(BaseArea=BaseArea*area.share, production=production*area.share) %>% group_by(NUTS_ID, SPECIES) %>% summarise(BaseArea=sum(BaseArea), production=sum(production))

### save res
saveRDS(crop.targets.GLOB, file = "input/croptargets_GLOBIOM.rds")

