#### CODE 3: prepare targets from CAPRI!

#### Author: Michael Wögerer
#### Date: 03/02/2025

## load CAPRI targets
capri.targets <- read.csv("input/CAPRI_CAPREG/preprocessed_CAPREG.csv", row.names = 1)
capri.targets <- capri.targets %>% mutate(year=as.character(year)) %>% filter(CAPRI_code==capri.reg, year==curr.year)
capri.targets <- capri.targets %>% left_join(crop_mapping %>% rename("crop"="CAPRI")) %>% na.omit() %>% group_by(CAPRI_code, type, GLOBIOM) %>% summarise(value=sum(as.numeric(value)))


capri.targets.area <- capri.targets %>% ungroup() %>% filter(type=="LEVL") %>% dplyr::select(GLOBIOM, value) %>% mutate(value=value*1000/100)
capri.targets.prod <- capri.targets %>% ungroup() %>% filter(type=="PROD") %>% dplyr::select(GLOBIOM, value)



#### add GLOBIOM targets
GLOB.targets <- readRDS("input/croptargets_GLOBIOM.rds")
GLOB.targets <- GLOB.targets %>% filter(NUTS_ID==lama.reg)
GLOB.targets.area <- GLOB.targets %>% ungroup() %>% mutate(value=BaseArea/100, GLOBIOM=SPECIES)%>% dplyr::select(GLOBIOM, value)
GLOB.targets.prod <- GLOB.targets %>% ungroup() %>% mutate(value=production/1000, GLOBIOM=SPECIES)%>% dplyr::select(GLOBIOM, value)



if(mixed.targets){

  targets.area <- full_join(GLOB.targets.area %>% rename('value2'='value'),capri.targets.area)
  targets.area[is.na(targets.area)]=0
  targets.area <- targets.area %>% mutate(value=ifelse(value==0,value2, value)) %>% dplyr::select(-value2)

  targets.prod <- full_join(GLOB.targets.prod %>% rename('value2'='value'),capri.targets.prod)
  targets.prod[is.na(targets.prod)]=0
  targets.prod <- targets.prod %>% mutate(value=ifelse(value==0,value2, value)) %>% dplyr::select(-value2)
} else {
  if(GLOB.target){
    if(nrow(GLOB.targets.area)==0){
      GLOB.targets.area <- capri.targets.area
      GLOB.targets.area$value <- 0
      GLOB.targets.prod <- capri.targets.prod
      GLOB.targets.prod$value <- 0


    }
    targets.area <- GLOB.targets.area
    targets.prod <- GLOB.targets.prod
  } else {
    targets.area <- capri.targets.area
    targets.prod <- capri.targets.prod
  }
}




### if yields not avialable make targets 0
which_zero <- final_yields %>% group_by(GLOBIOM_abr) %>% summarise(sum(YLD)) %>% na.omit()
which_zero <- setdiff(targets.area$GLOBIOM, which_zero$GLOBIOM_abr)


iii <- which_zero[1]
for(iii in which_zero){
targets.area$value[which(targets.area$GLOBIOM==iii)] <- 0
targets.prod$value[which(targets.prod$GLOBIOM==iii)] <- 0
}






##### add priors for positive targets and avialable yields

to.correct <- intersect(targets.area$GLOBIOM[targets.area$value!=0] , colnames(col_sums_temp)[col_sums_temp==0])

jjj <- "Rape"
#### prior check
for(jjj in to.correct){
  update.prior <- crop.area %>% dplyr::select(-NUTS_ID)
  colnames(update.prior)[2] <- jjj
  save.priors <- save.priors %>% dplyr::select(-any_of(jjj)) %>% left_join(update.prior)
}

if(length(to.correct)!=0){
  crop_cols <- setdiff(names(save.priors), "CELLCODE")

  # Row-wise sum of the crop columns
  save.priors[, total := rowSums(.SD), .SDcols = crop_cols]

  # Rescale each crop column by the row total
  save.priors[, (crop_cols) := lapply(.SD, function(x) x / total), .SDcols = crop_cols]

  # Remove the temporary 'total' column
  save.priors[, total := NULL]
}





