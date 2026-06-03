LU_map <- read_gms_data("input/mappings/SimUID_map.gms",header.line = 30, data.line = c(32,212738))
LU_map <- LU_map %>% filter(!grepl("_Any", AltiClass))

AEZ_map <- read_gms_data("input/mappings/set_simuAEZmap.gms",header.line = 1, data.line = c(3,211672))

final.simu <- LU_map %>% left_join(AEZ_map %>% rename("ALLCOUNTRY"="COUNTRY", "ColRow30"="AllColRow"))



LUID_map <- read_gms_data("input/mappings/SimUIDlu_map.gms",header.line = 31, data.line = c(33,425446))
LUID_map <- LUID_map %>% filter(!grepl("_Any", AltiClass)) %>% rename("AllColRow"="ALLCOLROW", "ANYREGION"="ALLCOUNTRY")


final.simu <- final.simu %>% left_join(LUID_map)

saveRDS(final.simu, file = "input/SimU_CR_LU_map.rds")
