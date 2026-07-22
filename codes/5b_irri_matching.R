
## load irrigation raster
irri.rast <- terra::rast(irri.path)
irri.rast <- crop(irri.rast, ext(temp.EEA))

## overlay of 1km² country map with irrigation class
temp.irri <- exactextractr::exact_extract(irri.rast, temp.EEA, force_df=T)
names(temp.irri) <- temp.EEA$CELLCODE
temp.irri <- data.table::rbindlist(temp.irri, idcol = "CELLCODE")
temp.irri$value[is.na(temp.irri$value)] <- 0
temp.irri <- temp.irri[, .(area = sum(coverage_fraction/100, na.rm = TRUE)), by = .(CELLCODE, value)]
temp.irri <- dcast(
  temp.irri,
  CELLCODE ~ paste0("Class_", value),
  value.var = "area",
  fill = 0
)
setnames(
  temp.irri,
  old = c("Class_0", "Class_1", "Class_2", "Class_3", "Class_4",
          "Class_5", "Class_6", "Class_7", "Class_8", "Class_9"),
  new = c(
    "Class_10",                         # 0
    "Class_7",                         # 1 -> irrigation in CORINE, <25% ESA
    "Class_5",                         # 2 -> irrigation in CORINE, 25-50% ESA
    "Class_3",                         # 3 -> irrigation in CORINE, 50-75% ESA
    "Class_1",                         # 4 -> irrigation in CORINE, >75% ESA
    "Class_8",                         # 5 -> no irrigation in CORINE, <25% ESA
    "Class_6",                         # 6 -> no irrigation in CORINE, 25-50% ESA
    "Class_4",                         # 7 -> no irrigation in CORINE, 50-75% ESA
    "Class_2",                         # 8 -> no irrigation in CORINE, >75% ESA
    "Class_9"                          # 9 -> irrigation in CORINE, no irrigation in ESA
  ), skip_absent=TRUE
)

setnames(temp.irri, old = "CELLCODE", new = "ns")

irri.res <- result %>% left_join(temp.irri) %>% filter(Class_10!=1)

classes <- paste0("Class_", 1:10)

allocate_irri <- function(df, target) {

  # initialize output
  df$irrigated <- 0

  # crops without an irrigation target get no irrigation
  if (length(target) == 0 || is.na(target))
    target <- 0

  remaining <- target

  for (cl in classes) {

    # skip non-existing class columns
    if (!cl %in% names(df)) next

    # available crop area in this class
    avail <- sum(df$sum_value * df[[cl]], na.rm = TRUE)

    if (avail <= 0) next
    if (remaining <= 0) break

    if (remaining >= avail) {

      # irrigate the whole class
      df$irrigated <- df$irrigated + df$sum_value * df[[cl]]
      remaining <- remaining - avail

    } else {

      # irrigate only the required fraction of this class
      frac <- remaining / avail
      df$irrigated <- df$irrigated + df$sum_value * df[[cl]] * frac
      remaining <- 0
      break

    }
  }

  df
}


result.irri <- irri.res %>%
  group_by(lu.to) %>%
  group_modify(~{
    target <- targets.irri$irri.area[
      match(.y$lu.to, targets.irri$GLOBIOM)
    ]
    allocate_irri(.x, target)
  }) %>%
  ungroup()

result.irri <- result.irri %>% group_by(lu.to, ns) %>% summarise(irrigated.area=sum(irrigated))


class.cols <- grep("^Class_", names(irri.res), value = TRUE)

max.irri <- irri.res %>%
  mutate(
    max_irri = sum_value * rowSums(across(all_of(class.cols)), na.rm = TRUE)
  ) %>%
  group_by(lu.to) %>%
  summarise(
    max_irri = sum(max_irri, na.rm = TRUE),
    .groups = "drop"
  )

overview.irri <- max.irri %>% left_join(targets.irri %>% rename("lu.to"="GLOBIOM", "target.irri"="irri.area")) %>%
  left_join(result.irri %>% group_by(lu.to) %>% summarise(actual.irri=sum(irrigated.area, na.rm=T)))
