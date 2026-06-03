##### CODE 2: prepare targets from CAPRI

#### Author: Michael Wögerer
#### Date: 28/01/2025

## load CAPRI shape file
CAPRI_NUTS <- read_sf("input/CAPRI_NUTS/NUTS2SUP_NONuts2_TRNuts1_V5_2023.shp")
CAPRI.targets <- read.csv("input/CAPRI_CAPREG/preprocessed_CAPREG.csv", row.names = 1)
LAMA.NUTS <- read_sf("input/NUTS_LAMASUS/shp_nuts.shp")

codes <- unique(CAPRI.targets$CAPRI_code)
CAPRI_NUTS <- CAPRI_NUTS %>% filter(NUTS2SUP %in% codes)
codes <- CAPRI_NUTS$NUTS2SUP
nuts_trimmed <- str_replace(codes, "0+$", "")

CAPRI_LAMA_mapping <- NULL
jj <- 229
for(jj in 1:length(codes)){

curr.NUTS.CAPRI <- codes[jj]
curr.NUTS <- nuts_trimmed[jj]

if(substr(curr.NUTS.CAPRI,1,2)=="BL"){
  curr.NUTS <- gsub("BL","BE",nuts_trimmed[jj])
}

find.lvl.id <- CAPRI_NUTS %>% filter(substr(NUTS2SUP,1,2)==substr(curr.NUTS.CAPRI,1,2))
find.lvl.id <- find.lvl.id %>%
  mutate(nuts_trimmed = str_replace(NUTS2SUP, "0+$", ""))
level <- max(nchar(find.lvl.id$nuts_trimmed))-2

CAPRI_NUTS.temp <- CAPRI_NUTS %>% filter(NUTS2SUP==curr.NUTS.CAPRI)
LAMA.NUTS.temp <- LAMA.NUTS %>% filter(LEVL_CODE==level, CNTR_CODE==substr(curr.NUTS,1,2))


choose.reg <- st_intersection(CAPRI_NUTS.temp, LAMA.NUTS.temp)
if(nrow(choose.reg)==0) {cat(curr.NUTS, "not in data");next}
choose.reg$area <- as.numeric(st_area(choose.reg))
comp.area <- as.numeric(st_area(CAPRI_NUTS.temp))

# Find the index of the row in choose.reg with area closest to comp.area
temp.mapping.pointer <- which.min(abs(choose.reg$area - comp.area))

CAPRI_LAMA_mapping <- CAPRI_LAMA_mapping %>% bind_rows(data.frame(CAPRI_NUTS=curr.NUTS.CAPRI, LAMA_NUTS=choose.reg$NUTS_ID[temp.mapping.pointer]))

}


##### add country ireland as faulty
CAPRI_LAMA_mapping <- CAPRI_LAMA_mapping %>% bind_rows(data.frame(CAPRI_NUTS="IR000000", LAMA_NUTS="IE"))

#problem BG
CAPRI_LAMA_mapping <- CAPRI_LAMA_mapping %>% filter(substr(CAPRI_NUTS,1,2)!="BG")
CAPRI_LAMA_mapping <- CAPRI_LAMA_mapping %>% bind_rows(data.frame(CAPRI_NUTS="BG000000", LAMA_NUTS="BG"))

saveRDS(CAPRI_LAMA_mapping,"output/CAPRI_LAMA_NUTSMAP.rds")


