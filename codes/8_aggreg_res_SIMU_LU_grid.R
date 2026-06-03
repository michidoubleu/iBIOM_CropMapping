library(gdxrrw)
library(dplyr)
library(tidyr)
library(ggplot2)
igdx("C:/GAMS/49")

source("codes/S3_read_gms.R")
cropmap <- read.csv("./save.from.clusteroverload/CELLCODE_SimU_mapping.csv")

last.file <- list.files("output/aggregated/", full.names = T)
last.file <- last.file[grepl(as.character(max(unique(as.numeric(substr(last.file,19,22))))),last.file)]
load(last.file)
res.IDs <- unique(agg.res$res$CELLCODE)

cropmap <- cropmap %>%
  filter(CELLCODE %in% res.IDs) %>%
  distinct()

SimU.res <- agg.res$res %>% left_join(cropmap) %>% group_by(SimUID, LUM.class, lu.to, year) %>% summarise(value=sum(value))

full.map <- readRDS("input/SimU_CR_LU_map.rds")
full.map <- full.map %>% rename("AllColRow"="ColRow30", "ANYREGION"="ALLCOUNTRY", "AEZCLASS"="AezClass") %>% dplyr::select(-ALLCOLROW)

LUID.res <- SimU.res %>% left_join(full.map %>% mutate(SimUID=as.numeric(SimUID))) %>% na.omit() %>%
  group_by(ANYREGION, AllColRow, AltiClass, SlpClass, SoilClass, AEZCLASS, LUM.class, lu.to, year) %>% summarise(value=sum(value))

### first just area, then yields as well. also need to add yields for other crops for full result coverage
### at this stage also need to filter out countries that are not supposed to have results
### replace all lines that have updated data and then write the GAMS file again!
countries = c("Spain", "Portugal", "Germany", "Greece", "Italy", "Netherlands", "Hungary", "Bulgaria",
              "Romania", "Poland", "CzechRep", "UK", "France", "Austria", "Sweden", "Slovakia",
              "Slovenia", "Cyprus", "Denmark", "Finland", "Croatia", "Lithuania", "Estonia", "Malta",
              "Latvia", "Ireland", "Belgium")

LUID.res <- LUID.res %>% filter(ANYREGION %in% countries)

LUID.res <- LUID.res %>% mutate(ALLTECH = case_when(
  LUM.class== "M1.rf"~ "LI",
  LUM.class== "M2.rf"~ "LI",
  LUM.class== "M3.rf"~ "HI",
  LUM.class== "M4.rf"~ "HI",
  LUM.class== "M5.rf"~ "HI",
  TRUE ~ as.character(LUM.class))) %>%
  group_by(ANYREGION, AllColRow, AltiClass, SlpClass, SoilClass, lu.to, ALLTECH)%>%
  summarise(value=sum(value)/10) %>% mutate(SPECIES=lu.to, ALLITEM="BaseArea") %>%
  ungroup() %>% dplyr::select(-lu.to)  # Keep other values unchanged

saveRDS(LUID.res, file="output/simu_res/SimU_res.rds")


    #I want to aggregate across regions and sum up the values





#
# crop_file <- list.files("input/GLOBIOM/",pattern = "*.gdx", full.names = TRUE)
# crop.dat <- gdxrrw::rgdx.param(file.path(crop_file), symName = "CROP_DATA")
#
# testomap <- crop.dat %>% left_join(LUID.res)



### compare to original data
# test <- LUID.res %>% group_by(ANYREGION, lu.to) %>% summarise(sum(value))
#
#

# crop.dat <- crop.dat %>% mutate(across(where(is.factor), as.character)) %>% filter(SPECIES==ALLITEM|ALLITEM=="BaseArea", nchar(ALLTECH)==2) %>% mutate(ALLITEM=ifelse(ALLITEM=="BaseArea","BaseArea","Yield")) %>% pivot_wider(names_from=ALLITEM, values_from=CROP_DATA, values_fill = 0) %>% mutate(BaseArea=BaseArea*1000)
#
# test2 <- crop.dat %>% group_by(ANYREGION, SPECIES) %>% summarise(value=sum(BaseArea))
