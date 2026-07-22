library(gdxrrw)
library(dplyr)
library(tidyr)
library(ggplot2)
igdx("C:/GAMS/49")

source("codes/S3_read_gms.R")

cropmap <- read.csv(paste0(mapping.path,"/CELLCODE_SimU_mapping.csv"))

select.file <- list.files(paste0("output/",RESULT_TAG,"/aggregated/"), full.names = T)
select.file <- select.file[grepl(RESULT_TAG,select.file)]
load(select.file)
res.IDs <- unique(agg.res$res$CELLCODE)

cropmap <- cropmap %>%
  filter(CELLCODE %in% res.IDs) %>%
  distinct()

SimU.res <- agg.res$res %>% left_join(cropmap) %>% group_by(SimUID, LUM.class, lu.to, year) %>% summarise(value=sum(value))


full.map <- readRDS(paste0(mapping.path,"/SimU_CR_LU_map.rds"))
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
  LUM.class== "M5.ir"~ "IR",
  TRUE ~ as.character(LUM.class))) %>%
  group_by(ANYREGION, AllColRow, AltiClass, SlpClass, SoilClass, lu.to, ALLTECH)%>%
  summarise(value=sum(value)/10) %>% mutate(SPECIES=lu.to, ALLITEM="BaseArea") %>%
  ungroup() %>% dplyr::select(-lu.to) %>% filter(!(SPECIES=="OthAgr"&ALLTECH!="OthAgr"))  # Keep other values unchanged

res.123 <- LUID.res %>% group_by(ANYREGION, SPECIES, ALLTECH) %>% summarise(value=sum(value)) %>% pivot_wider(names_from = "ALLTECH", values_from = "value")

saveRDS(LUID.res, file=paste0("output/",RESULT_TAG,"/simu_res/",RESULT_TAG,"_SimU_res.rds"))



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
