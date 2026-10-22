##### CODE 15: aggregate results to SimU and GLOBIOM LU grid

library(gdxrrw)
library(dplyr)
library(tidyr)
library(ggplot2)
igdx(GAMS_PATH)


cropmap <- read.csv(paste0(mapping.path,"/CELLCODE_SimU_mapping.csv"))

select.file <- list.files(paste0("output/",RESULT_TAG,"/aggregated/"), full.names = T)
select.file <- select.file[grepl(RESULT_TAG,select.file)]
load(select.file)
res.IDs <- unique(agg.res$res$CELLCODE)

cropmap <- cropmap %>%
  filter(CELLCODE %in% res.IDs) %>%
  distinct()

### per-ha attributes (yield + additional variables) are aggregated area-weighted:
### carry sum(x*area) and sum(area where x available) through all aggregations, divide at the end
attr.vars <- intersect(c("YLD", extra_vars), names(agg.res$res))
attr.w    <- paste0("w.", attr.vars)

SimU.res <- agg.res$res %>% left_join(cropmap) %>%
  mutate(across(all_of(attr.vars), ~ ifelse(is.na(.x), 0, value), .names = "w.{.col}"),
         across(all_of(attr.vars), ~ coalesce(.x * value, 0))) %>%
  group_by(SimUID, LUM.class, lu.to, year) %>%
  summarise(across(c(value, all_of(attr.vars), all_of(attr.w)), sum), .groups = "drop")


full.map <- readRDS(SimU.map.path)
### GLOBIOM AllColRow uses the LU codes (ALLCOLROW), not the CR codes (ColRow30)
full.map <- full.map %>% rename("AllColRow"="ALLCOLROW", "ANYREGION"="ALLCOUNTRY", "AEZCLASS"="AezClass") %>% dplyr::select(-ColRow30)

LUID.res <- SimU.res %>% left_join(full.map %>% mutate(SimUID=as.numeric(SimUID))) %>% filter(!is.na(ANYREGION)) %>%
  group_by(ANYREGION, AllColRow, AltiClass, SlpClass, SoilClass, AEZCLASS, LUM.class, lu.to, year) %>%
  summarise(across(c(value, all_of(attr.vars), all_of(attr.w)), sum), .groups = "drop")

### first just area, then yields as well. also need to add yields for other crops for full result coverage
### at this stage also need to filter out countries that are not supposed to have results
### replace all lines that have updated data and then write the GAMS file again!
countries = c("Spain", "Portugal", "Germany", "Greece", "Italy", "Netherlands", "Hungary", "Bulgaria",
              "Romania", "Poland", "CzechRep", "UK", "France", "Austria", "Sweden", "Slovakia",
              "Slovenia", "Cyprus", "Denmark", "Finland", "Croatia", "Lithuania", "Estonia", "Malta",
              "Latvia", "Ireland", "Belgium", "Luxembourg")

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
  summarise(across(c(value, all_of(attr.vars), all_of(attr.w)), sum), .groups = "drop")

# back from area-weighted sums to per-ha values
for (v in attr.vars) LUID.res[[v]] <- LUID.res[[v]] / LUID.res[[paste0("w.", v)]]

LUID.res <- LUID.res %>%
  mutate(BaseArea = value/10, SPECIES = lu.to) %>%
  dplyr::select(-lu.to, -value, -all_of(attr.w)) %>% filter(!(SPECIES=="OthAgr"&ALLTECH!="OthAgr")) %>%  # Keep other values unchanged
  ### long GLOBIOM CROP_DATA style: ALLITEM = BaseArea, <SPECIES> (yield), FTN, IRGA, ...
  pivot_longer(c(BaseArea, all_of(attr.vars)), names_to = "ALLITEM", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(ALLITEM = ifelse(ALLITEM == "YLD", SPECIES, ALLITEM))

res.123 <- LUID.res %>% filter(ALLITEM == "BaseArea") %>% group_by(ANYREGION, SPECIES, ALLTECH) %>% summarise(value=sum(value)) %>% pivot_wider(names_from = "ALLTECH", values_from = "value")

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
