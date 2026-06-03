require(fuzzyjoin)

eurostat_GLOBIOM_mapping <- eurostat_GLOBIOM_mapping %>% mutate(crops_temp=gsub("0","",eurostat_GLOBIOM_mapping$crops))
eurostat_crop <- eurostat::get_eurostat("apro_cpshr")
eurostat_time_span <- c("2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01","2021-01-01")

eurostat_crop_tot_area <- eurostat_crop %>% mutate(TIME_PERIOD=as.character(TIME_PERIOD)) %>%
  dplyr::filter(crops %in% c("ARA", "UAA"),
         #strucpro %in% c("AR"),
         TIME_PERIOD %in% eurostat_time_span) %>%
      rename(time="TIME_PERIOD") %>% mutate(time=as.character(time))

eurostat_crop <- eurostat_crop %>%
                      filter(
                        grepl(paste(eurostat_GLOBIOM_mapping$crops_temp,collapse="|"),crops),
                        strucpro %in% c("AR"),
                        TIME_PERIOD %in% eurostat_time_span)



eurostat_crop <- regex_left_join(eurostat_crop,
                                 eurostat_GLOBIOM_mapping %>%
                                   select(c(EPIC_abr,crops_temp)), by = c("crops" = "crops_temp")) %>%
                                    select(!crops_temp) %>% na.omit(EPIC_abr)

# eurostat_crop_NUTS0 <- eurostat_crop %>% filter(nchar(geo)==2) %>% pivot_wider(names_from=c(TIME_PERIOD,crops),values_from=values) %>%
#                         pivot_longer(cols=!c(freq,strucpro,geo),names_to=c("time","crops"), names_sep="_",values_to="value")
# eurostat_crop_NUTS0 <- eurostat_crop_NUTS0 %>%
#                          mutate(level=ifelse(as.integer(substr(crops,4,4))!=0,"level3",
#                                                                    ifelse(as.integer(substr(crops,4,4))==0 & as.integer(substr(crops,3,3))!=0,"level2",
#                                                                           ifelse(as.integer(substr(crops,3,3))==0 & as.integer(substr(crops,2,2))!=0,"level1",
#                                                                                "level0"))),
#                                 class=substr(crops,1,1))
#
# eurostat_crop_NUTS0 <- eurostat_crop_NUTS0 %>% pivot_wider(names_from="level",values_from="value")
# eurostat_crop_NUTS1 <- eurostat_crop %>% filter(nchar(geo)==3) %>% pivot_wider(names_from=crops,values_from=values)
#%>% filter(nchar(geo)==4)
eurostat_crop <- eurostat_crop %>% pivot_wider(names_from=c(TIME_PERIOD,crops,EPIC_abr),names_sep="%",values_from=values) %>%
                        pivot_longer(cols=!c(freq,strucpro,geo),names_to=c("time","crops","EPIC_abr"), names_sep="%",values_to="value")

eurostat_crop <- eurostat_crop %>%
  mutate(level=ifelse(as.integer(substr(crops,5,5))!=0,"level4",
                      ifelse(as.integer(substr(crops,5,5))==0 & as.integer(substr(crops,4,4))!=0,"level3",
                             ifelse(as.integer(substr(crops,4,4))==0 & as.integer(substr(crops,3,3))!=0,"level2",
                                    ifelse(as.integer(substr(crops,3,3))==0 & as.integer(substr(crops,2,2))!=0,"level1",
                                      "level0"))))) %>% filter(crops!="I1110-1130",
                                                               geo!="EL41_42",
                                                               !grepl(paste(c("EL1","EL2"),collapse="|"),geo),
                                                               level!="level0")

eurostat_crop <- eurostat_crop %>% pivot_wider(names_from="level",values_from="value")
eurostat_crop  <- eurostat_crop %>% group_by(geo,time,EPIC_abr) %>%
                        mutate(level3=ifelse(crops=="C1320",sum(level2,na.rm=TRUE)-sum(level3[crops=="C1310"],na.rm=TRUE),level3),
                               level4=ifelse(crops=="C1120",level3,level4)) %>% ungroup()


eurostat_crop <- eurostat_crop  %>%
                        group_by(geo,time,EPIC_abr) %>% mutate(value=ifelse(sum(level4,na.rm=TRUE)!=0,sum(level4,na.rm=TRUE),NA),
                                                               value=ifelse(sum(level3,na.rm=TRUE)!=0 & all(is.na(value)),sum(level3,na.rm=TRUE),value),
                                                               value=ifelse(sum(level2,na.rm=TRUE)!=0 & all(is.na(value)),sum(level2,na.rm=TRUE),value),
                                                               value=ifelse(sum(level1,na.rm=TRUE)!=0 & all(is.na(value)),sum(level1,na.rm=TRUE),value)) %>% ungroup()

eurostat_crop <- eurostat_crop %>% group_by(geo,time,EPIC_abr) %>% reframe(value=mean(value)) %>%
                        pivot_wider(names_from=c("EPIC_abr","time"),names_sep="%",values_from="value") %>%
                          pivot_longer(cols=!c(geo),names_to=c("EPIC_abr","time"),names_sep="%",values_to="value")


# match EPIC crop area with total area and obtain "other class"
eurostat_crop_EPIC_resid_area <- eurostat_crop %>% group_by(geo, time) %>% reframe(tot_EPIC_crop_area =
                                                                                            ifelse(all(is.na(value)),NA,sum(value,na.rm=TRUE))) %>%
  left_join(
    eurostat_crop_tot_area %>% filter(crops == "ARA")  %>% group_by(geo,time) %>% reframe(tot_eurostat_area =
                                                                                                         sum(values))
  ) %>%
  mutate(value = tot_eurostat_area - tot_EPIC_crop_area, EPIC_abr = "OTHER") %>% select(!c(tot_EPIC_crop_area, tot_eurostat_area))
print(knitr::kable(eurostat_crop_EPIC_resid_area %>% filter(value < 0),format="simple"))
eurostat_crop_EPIC_resid_area <- eurostat_crop_EPIC_resid_area %>% mutate(value =
                                                                ifelse(value < 0, 0, value))
eurostat_crop <- bind_rows(eurostat_crop_EPIC_resid_area, eurostat_crop)

eurostat_crop_NUTS2 <- eurostat_crop %>% filter(nchar(geo)==4) %>% mutate(NUTS2=geo,NUTS1=substr(geo,1,3),NUTS0=substr(geo,1,2)) %>% select(!geo) %>%
  pivot_wider(names_from=c("EPIC_abr","time"),names_sep="%",values_from="value") %>%
  pivot_longer(cols=!c(NUTS2,NUTS1,NUTS0),names_to=c("EPIC_abr","time"),names_sep="%",values_to="value")

# eurostat_crop_NUTS2 <- eurostat_crop_NUTS2 %>% group_by(NUTS2,EPIC_abr) %>% mutate(share=ifelse(all(is.na(value)),NA,sum(value,na.rm=TRUE))) %>% group_by(NUTS2,time) %>% mutate(share=ifelse(all(is.na(share)),NA,share/sum(share,na.rm=TRUE)))


eurostat_crop_NUTS1 <- eurostat_crop %>% filter(nchar(geo)==3) %>% mutate(NUTS1=substr(geo,1,3),NUTS0=substr(geo,1,2)) %>% select(!geo) %>%
  pivot_wider(names_from=c("EPIC_abr","time"),names_sep="%",values_from="value") %>%
  pivot_longer(cols=!c(NUTS1,NUTS0),names_to=c("EPIC_abr","time"),names_sep="%",values_to="value")


eurostat_crop_NUTS0 <- eurostat_crop %>% filter(nchar(geo)==2) %>% mutate(NUTS0=substr(geo,1,2)) %>% select(!geo) %>%
  pivot_wider(names_from=c("EPIC_abr","time"),names_sep="%",values_from="value") %>%
  pivot_longer(cols=!c(NUTS0),names_to=c("EPIC_abr","time"),names_sep="%",values_to="value")

eurostat_crop_NUTS2 <- left_join(eurostat_crop_NUTS2,eurostat_crop_NUTS1, by=c("NUTS1","NUTS0","EPIC_abr","time")) %>% group_by(NUTS1,EPIC_abr,time) %>% mutate(value.x=ifelse(is.na(value.x) & !is.na(value.y),value.y/n(),value.x)) %>% select(!value.y) %>% rename(value="value.x")

eurostat_crop_NUTS2 <- left_join(eurostat_crop_NUTS2,eurostat_crop_NUTS0, by=c("NUTS0","EPIC_abr","time")) %>% group_by(NUTS0,EPIC_abr,time) %>% mutate(value.x=ifelse(is.na(value.x) & !is.na(value.y),value.y/n(),value.x)) %>% select(!value.y) %>% rename(value="value.x")

eurostat_crop_merge <- left_join(eurostat_crop_NUTS2,eurostat_crop_NUTS1,by=c("time","NUTS1","NUTS0","EPIC_abr")) %>%
                        left_join(eurostat_crop_NUTS0,by=c("time","NUTS0","EPIC_abr"))
