source("./codes/F1_excess_distribution.R")

#final.alloc <- NULL

# ## load binary irrigation raster
# irri.rast <- terra::rast(irrig.path)
# # Try to crop irri.rast, proceed only if successful
# irri.rast.cropped <- tryCatch({
#   crop(irri.rast, ext(temp.EEA))
# }, error = function(e) {
#   message("Irrigation failed: ", e$message)
#   return(NULL)
# })

irri.rast.cropped <- NULL

if (!is.null(irri.rast.cropped)) {
  irri.rast <- irri.rast.cropped

  ## overlay of 1km² country map and 100m LUM map
  temp.irri <- exactextractr::exact_extract(irri.rast, temp.EEA, force_df = TRUE)
  names(temp.irri) <- temp.EEA$CELLCODE
  temp.irri <- bind_rows(temp.irri, .id = "CELLCODE")
  temp.irri <- as.data.table(temp.irri)
  temp.irri <- temp.irri[, .(area = sum(coverage_fraction / 100, na.rm = TRUE)), by = .(CELLCODE, value)]
  temp.irri <- temp.irri[!is.nan(value)]
  setnames(temp.irri, old = "value", new = "irrigation")

  temp.irri <- as.data.frame(temp.irri)
  temp.irri <- temp.irri %>% filter(irrigation == 1)

  temp.LUM %>% left_join(temp.irri)

  irri.perc <- crop.area %>%
    left_join(temp.irri) %>%
    mutate(irri.perc = ifelse(is.na(area / total_area), 0, area / total_area)) %>%
    mutate(irri.perc = ifelse(irri.perc > 1, 1, irri.perc))

  crop.ares.LUM <- temp.LUM %>% group_by(CELLCODE) %>% mutate(total=sum(total_area), share=total_area/total) %>% dplyr::select(CELLCODE, LUM.class, share) %>% full_join(result %>% rename("CELLCODE"="ns")) %>% mutate(value=share*sum_value) %>% dplyr::select(CELLCODE, LUM.class, lu.to, value)%>%
    mutate(LUM.class = case_when(
      LUM.class %in% lum_crop_map[c(1,6)] ~ "M1.rf",
      LUM.class %in% lum_crop_map[c(2,7)] ~ "M2.rf",
      LUM.class %in% lum_crop_map[c(3,8)] ~ "M3.rf",
      LUM.class %in% lum_crop_map[c(4,9)] ~ "M4.rf",
      LUM.class %in% lum_crop_map[c(5,10)] ~ "M5.rf",
      TRUE ~ as.character(LUM.class)  # Keep other values unchanged
    )) %>% left_join(temp.irri)

} else {
  crop.ares.LUM <- temp.LUM %>% group_by(CELLCODE) %>% mutate(total=sum(total_area), share=total_area/total) %>% dplyr::select(CELLCODE, LUM.class, share) %>% full_join(result %>% rename("CELLCODE"="ns")) %>% mutate(value=share*sum_value) %>% dplyr::select(CELLCODE, LUM.class, lu.to, value)%>%
    mutate(LUM.class = case_when(
      LUM.class %in% lum_crop_map[c(1,6)] ~ "M1.rf",
      LUM.class %in% lum_crop_map[c(2,7)] ~ "M2.rf",
      LUM.class %in% lum_crop_map[c(3,8)] ~ "M3.rf",
      LUM.class %in% lum_crop_map[c(4,9)] ~ "M4.rf",
      LUM.class %in% lum_crop_map[c(5,10)] ~ "M5.rf",
      TRUE ~ as.character(LUM.class)  # Keep other values unchanged
    ))
}

temp.prod <- crop.ares.LUM %>% left_join(final_yields %>% rename("LUM.class"="Scen", "lu.to"="GLOBIOM_abr")) %>% mutate(prod=value*100*YLD)


temp.prod <- temp.prod %>% group_by(lu.to) %>% summarise(current.prod=sum(prod, na.rm = TRUE))

if(sum(temp.prod$current.prod)!=0){

target.prod <- targets.prod %>% mutate(value=value*1000)

combi.grid.pos <- data.frame(LUM.from=c("M1.rf","M2.rf","M3.rf","M4.rf","M5.rf"), LUM.to=c("M2.rf","M3.rf","M4.rf","M5.rf","M5.rf"))
combi.grid.neg <- data.frame(LUM.to=c("M1.rf","M1.rf","M2.rf","M3.rf","M4.rf", "M5.rf"), LUM.from=c("M1.rf","M2.rf","M3.rf","M4.rf","M5.rf", "M5.ir"))

##### adjustment for production in a loop
adj.crops <- temp.prod$lu.to[temp.prod$current.prod>0]
curr.crop <- "Barl"

final.alloc.list <- list()

for(curr.crop in adj.crops){

  curr.crop.areas <- crop.ares.LUM %>%  filter(lu.to==curr.crop)
  
  yields_curr <- final_yields %>% filter(GLOBIOM_abr == curr.crop) %>% rename(LUM.class = Scen, lu.to = GLOBIOM_abr)
  yields_curr_from <- yields_curr %>% dplyr::select(CELLCODE, LUM.class, YLD) %>% rename(LUM.from = LUM.class, YLD.from = YLD)
  yields_curr_to <- yields_curr %>% dplyr::select(CELLCODE, LUM.class, YLD) %>% rename(LUM.to = LUM.class, YLD.to = YLD)

do.again <- TRUE
count <- 1
while(do.again && count<5){
  do.again <- TRUE

  curr.crop.prod <- curr.crop.areas %>% left_join(yields_curr, by = c("CELLCODE", "LUM.class", "lu.to")) %>% mutate(prod=value*100*YLD) %>% group_by(lu.to) %>% summarise(current.prod=sum(prod, na.rm = TRUE))

  temp.crop.prod <- curr.crop.prod$current.prod
  target.crop.prod <- target.prod$value[target.prod$GLOBIOM==curr.crop]

  curr.crop.diff <- target.crop.prod-temp.crop.prod


if(curr.crop.diff<0){



curr.prod.per.LUM <- curr.crop.areas %>% left_join(yields_curr, by = c("CELLCODE", "LUM.class", "lu.to")) %>% mutate(prod=value*100*YLD) %>% group_by(LUM.class) %>% summarise(current.prod=sum(prod, na.rm = TRUE)) %>% filter(LUM.class!="M1.rf") %>% mutate(prod.share=current.prod/sum(current.prod,na.rm=T))

reduction <- curr.prod.per.LUM
reduction$to.reduce <- reduction$prod.share*abs(curr.crop.diff)

potential.diff <- yields_curr_from %>% full_join(combi.grid.neg, by = "LUM.from") %>% left_join(yields_curr_to, by = c("CELLCODE", "LUM.to")) %>% na.omit() %>% mutate(YLD.diff=YLD.from-YLD.to) %>% left_join(curr.crop.areas %>% left_join(yields_curr, by = c("CELLCODE", "LUM.class", "lu.to")) %>% mutate(prod=value*100*YLD) %>% filter(lu.to==curr.crop)%>%dplyr::select(CELLCODE, LUM.class,value) %>% rename("LUM.from"="LUM.class", "area"="value"), by = c("CELLCODE", "LUM.from")) %>% na.omit()
if(all(potential.diff$YLD.diff<0.001)){warning(paste0("No yield diff in ", curr.crop));do.again<-FALSE;next}

potential.diff <- potential.diff %>% mutate(change.max=area*YLD.diff*100)

area.reduction.max <- potential.diff %>% group_by(LUM.from) %>% summarise(tot.max.change=sum(change.max)) %>% left_join(reduction %>% dplyr::select(LUM.class, to.reduce) %>% rename("LUM.from"="LUM.class")) %>% mutate(actual=to.reduce/tot.max.change) %>% na.omit()

if(any(area.reduction.max$actual>1)){
  area.reduction.max <- redistribute_reduction(area.reduction.max)
} else if(any(area.reduction.max<0)){
  area.reduction.max <- redistribute_reduction(area.reduction.max)
}


old.areas <- curr.crop.areas %>% filter(lu.to==curr.crop) %>% left_join(area.reduction.max %>% dplyr::select(LUM.from, actual) %>% rename("LUM.class"="LUM.from")) %>% mutate(actual=ifelse(is.na(actual),0,actual),value=value*(1-actual)) %>% dplyr::select(-actual)

new.areas <- curr.crop.areas %>% filter(lu.to==curr.crop) %>% left_join(area.reduction.max %>% dplyr::select(LUM.from, actual) %>% rename("LUM.class"="LUM.from")) %>% left_join(combi.grid.neg %>% rename('LUM.class'="LUM.from")) %>% mutate(actual=ifelse(is.na(actual),0,actual),value=value*actual) %>% dplyr::select(-actual, -LUM.class) %>% rename("LUM.class"="LUM.to")


new.crop.areas <- old.areas %>% bind_rows(new.areas)%>% filter(value!=0)
} else {

  curr.prod.per.LUM <- curr.crop.areas %>% left_join(yields_curr, by = c("CELLCODE", "LUM.class", "lu.to")) %>% mutate(prod=value*100*YLD) %>% filter(lu.to==curr.crop) %>% group_by(LUM.class) %>% summarise(current.prod=sum(prod, na.rm = TRUE)) %>% filter(LUM.class!="M5.rf") %>% mutate(prod.share=current.prod/sum(current.prod,na.rm=T))

  increase <- curr.prod.per.LUM
  increase$to.increase <- increase$prod.share*abs(curr.crop.diff)

  potential.diff <- yields_curr_from %>% full_join(combi.grid.pos, by = "LUM.from") %>% left_join(yields_curr_to, by = c("CELLCODE", "LUM.to")) %>% na.omit() %>% mutate(YLD.diff=YLD.to-YLD.from) %>% left_join(curr.crop.areas %>% left_join(yields_curr, by = c("CELLCODE", "LUM.class", "lu.to")) %>% mutate(prod=value*100*YLD) %>% filter(lu.to==curr.crop)%>%dplyr::select(CELLCODE, LUM.class,value) %>% rename("LUM.from"="LUM.class", "area"="value"), by = c("CELLCODE", "LUM.from")) %>% na.omit()

  if(all(potential.diff$YLD.diff<0.001)){warning(paste0("No yield diff in ", curr.crop));do.again<-FALSE;next}

  potential.diff <- potential.diff %>% mutate(change.max=area*YLD.diff*100)

  area.increase.max <- potential.diff %>% group_by(LUM.from) %>% summarise(tot.max.change=sum(change.max)) %>% left_join(increase %>% dplyr::select(LUM.class, to.increase) %>% rename("LUM.from"="LUM.class")) %>% mutate(actual=to.increase/tot.max.change) %>% na.omit

  if(any(area.increase.max$actual>1)){
    area.increase.max <- redistribute_excess(area.increase.max)
  } else if(any(area.increase.max<0)){
    area.increase.max <- redistribute_excess(area.increase.max)
  }


  old.areas <- curr.crop.areas %>% filter(lu.to==curr.crop) %>% left_join(area.increase.max %>% dplyr::select(LUM.from, actual) %>% rename("LUM.class"="LUM.from")) %>% mutate(actual=ifelse(is.na(actual),0,actual),value=value*(1-actual)) %>% dplyr::select(-actual)


  new.areas <- curr.crop.areas %>% filter(lu.to==curr.crop) %>% left_join(area.increase.max %>% dplyr::select(LUM.from, actual) %>% rename("LUM.class"="LUM.from")) %>% left_join(combi.grid.pos %>% rename('LUM.class'="LUM.from")) %>% mutate(actual=ifelse(is.na(actual),0,actual),value=value*actual) %>% dplyr::select(-actual, -LUM.class) %>% rename("LUM.class"="LUM.to")

  new.crop.areas <- old.areas %>% bind_rows(new.areas)%>% filter(value!=0)

}

  updated.prod <- new.crop.areas %>% left_join(yields_curr, by = c("CELLCODE", "LUM.class", "lu.to")) %>% mutate(prod=value*100*YLD)
  updated.prod <- updated.prod %>% group_by(lu.to) %>% summarise(current.prod=sum(prod, na.rm = TRUE))

if(abs((updated.prod$current.prod/target.crop.prod)-1)<0.001){
  do.again <- FALSE
}
if(length(unique(new.crop.areas$LUM.class))==1){
  do.again <- FALSE
}

  curr.crop.areas <- new.crop.areas

count <- count+1
}

if(any(curr.crop.areas$value<0)){
  curr.crop.areas <- crop.ares.LUM %>%  filter(lu.to==curr.crop)
}

final.alloc.list[[curr.crop]] <- curr.crop.areas
}

final.alloc <- final.alloc %>% bind_rows(bind_rows(final.alloc.list))
} else {
  final.alloc <- crop.ares.LUM
}

final.alloc$region <- lama.reg
final.alloc$year <- curr.year

final.areas <- final.alloc %>% group_by(lu.to) %>% summarise(final.area=sum(value))
final.areas <- final.areas %>% rename("GLOBIOM"="lu.to") %>% left_join(targets.area %>% rename("target.area"="value")) %>% mutate(area.diff=final.area-target.area)


final.prod <- final.alloc %>% left_join(final_yields %>% rename("LUM.class"="Scen", "lu.to"="GLOBIOM_abr")) %>% mutate(prod=value*100*YLD/1000)
final.prod <- final.prod %>% group_by(lu.to) %>% summarise(final.prod=sum(prod, na.rm = TRUE))
final.prod <- final.prod %>% rename("GLOBIOM"="lu.to") %>% left_join(targets.prod %>% rename("target.prod"="value")) %>% mutate(prod.diff=final.prod-target.prod)

show.res <- final.areas %>% left_join(final.prod)
show.res$region <- lama.reg
show.res$year <- curr.year
