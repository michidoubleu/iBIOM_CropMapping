##### CODE 10: irrigation matching — binned irrigated-area likelihood (Bonn layer)
## load irrigation raster
irri.rast <- terra::rast(irri.path)
irri.rast <- crop(irri.rast, ext(temp.EEA))


## extract raster values using cell centroids for extreme performance on perfectly aligned 1x1km grids
pts <- sf::st_centroid(temp.EEA)
temp.irri <- terra::extract(irri.rast, terra::vect(pts))
temp.irri$CELLCODE <- temp.EEA$CELLCODE
temp.irri$ID <- NULL
temp.irri <- data.table::as.data.table(temp.irri)
temp.irri <- temp.irri[CELLCODE%in%unique.CELLCODES]

# Binned likelihood classes from the irrigated area per 1 km² cell (0-100 ha):
#   Class_10 = 91-100 ha (most likely), Class_9 = 81-90 ha, ..., Class_1 = 1-10 ha
#   0 ha (or no data) excludes the cell from receiving any irrigation
temp.irri <- temp.irri[!is.na(Total_IR_A_2010) & Total_IR_A_2010 > 0]
temp.irri[, IR_class := paste0("Class_", pmin(ceiling(pmin(Total_IR_A_2010, 100) / 10), 10L))]

# Class column = irrigated share of the cell (caps the irrigable crop area in that cell)
classes <- paste0("Class_", 10:1)

if (nrow(temp.irri) > 0) {
  temp.irri <- dcast(
    copy(temp.irri)[
      , Total_IR_A_2010 := Total_IR_A_2010 / 100
    ],
    CELLCODE ~ IR_class,
    value.var = "Total_IR_A_2010",
    fill = 0
  )
} else {
  temp.irri <- data.table(CELLCODE = character())
}
for (cl in setdiff(classes, names(temp.irri))) temp.irri[, (cl) := numeric()]

# Fallback once all classes are exhausted: the non-irrigated share of cells with irrigation > 0
temp.irri[, Class_rest := pmax(0, 1 - rowSums(.SD, na.rm = TRUE)), .SDcols = classes]

setnames(temp.irri, old = "CELLCODE", new = "ns")

irri.res <- result %>% inner_join(temp.irri, by = "ns")

# allocation order: most likely class first, residual last
classes <- c(classes, "Class_rest")

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
