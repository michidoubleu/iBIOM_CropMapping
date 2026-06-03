##### CODE 5: EPIC yields prepare

#### Author: Michael Wögerer
#### Date: 07/02/2025


source("input/mappings/FAO_eurostat_EPIC_GLOBIOM_crop_mapping.R")

YLDG_files <- list.files("./input/EPIC/YLDG", full.names = T)
YLDG_files <- grep("REF|ReadMe",YLDG_files,inv=TRUE,value=TRUE)
YLDG <- YLDG_files[1]
YLDG_data <- NULL
for (YLDG in YLDG_files) {
  temp <- read.csv(paste0(YLDG))

  YLDG_data <- bind_rows(YLDG_data, temp)
  YLDG <- NULL

}

DM_conversion <- read.csv("./input/EPIC/DM_content.csv")

YLDG_data <- YLDG_data %>% left_join(
  DM_conversion %>%
    left_join(eurostat_GLOBIOM_mapping, by =
                c("CROP" = "GLOBIOM_abr")) %>%
    mutate(DM_factor_inv = 1 / value, CROP =
             EPIC_abr) %>% select(CROP, DM_factor_inv)
) %>% mutate(YLD = YLDG * DM_factor_inv) %>% select(!c(DM_factor_inv, YLDG))

final_yields <-YLDG_data %>% dplyr::select(SimUID, Scen, CROP, YLD)


saveRDS(final_yields, file="output/EPIC_yields.rds")

