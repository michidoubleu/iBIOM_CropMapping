library(gdxrrw)
library(dplyr)
library(tidyr)
library(ggplot2)
igdx("C:/GAMS/40")

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
final.cropdat <- crop.dat.proc %>% group_by(SimUID, ALLTECH, SPECIES) %>% summarise(BaseArea=sum(BaseArea), production=sum(production))

final.cropdat <- final.cropdat %>% ungroup() %>%
  mutate(
    YLD = production / BaseArea,
    Scen = case_when(
      ALLTECH == "HI" ~ "M5.rf",
      ALLTECH == "LI" ~ "M3.rf",
      ALLTECH == "SS" ~ "M1.rf",
      ALLTECH == "IR" ~ "M5.ir",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na() %>%
  select(-production, -BaseArea, -ALLTECH)

final.cropdat <- final.cropdat %>%
  bind_rows(final.cropdat %>% filter(Scen=="M5.rf") %>% mutate(Scen = "M4.rf")) %>%
  bind_rows(final.cropdat %>% filter(Scen=="M3.rf") %>% mutate(Scen = "M2.rf")) %>% rename("GLOBIOM_abr"= "SPECIES")


saveRDS(final.cropdat, file = "input/cropyieldsOLD_GLOBIOM.rds")

