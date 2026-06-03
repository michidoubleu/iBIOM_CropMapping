##### CODE 4: Area target matching with downscalR

#### Author: Michael Wögerer
#### Date: 08/01/2025

tot.area <- sum(crop.area$total_area)
tot.target <- sum(targets.area$value)

if(tot.area-tot.target<0){ warning("Problem with total area OtherAgri smaller than 0!")
  targets.area$value <- targets.area$value*(1-abs((tot.area-tot.target)/sum(targets.area$value)))
  tot.target <- sum(targets.area$value)
}

ds.targets <- data.frame(times=curr.year, targets.area)

ds.targets <- ds.targets %>% bind_rows(data.frame(times=curr.year,GLOBIOM="OthAgr", value=tot.area-tot.target)) %>% rename("lu.to"="GLOBIOM")

ds.priors <- save.priors
ds.priors <- melt(
  ds.priors,
  id.vars = "CELLCODE",       # Keep CELLCODE as an identifier
  variable.name = "lu.to",  # Name for the variable column
  value.name = "value"        # Name for the value column
)
ds.priors <- ds.priors %>% filter(lu.to %in% ds.targets$lu.to)

ds.priors <- as.data.frame(ds.priors)
ds.priors$lu.to <- as.character(ds.priors$lu.to)
colnames(ds.priors)[1] <- "ns"

ds.start.areas <- crop.area %>% dplyr::select(CELLCODE, total_area) %>% as.data.frame() %>% rename("value"="total_area", "ns"="CELLCODE")



### fix to correct very small negative diff to one other target
if(ds.targets$value[ds.targets$lu.to=="OthAgr"]<0){
  ds.targets$value[which(ds.targets$value>0)[1]] <- ds.targets$value[which(ds.targets$value>0)[1]] + ds.targets$value[ds.targets$lu.to=="OthAgr"]
  ds.targets$value[ds.targets$lu.to=="OthAgr"] <- 0
} else {
  diff <- sum(ds.targets$value)-sum(ds.start.areas$value)
  ds.targets$value[ds.targets$lu.to=="OthAgr"] <- ds.targets$value[ds.targets$lu.to=="OthAgr"] - diff
}

### fix to check if priors are available for all ns
if(length(unique(ds.start.areas$ns))!=length(unique(ds.priors$ns))){
  missing <- setdiff(ds.start.areas$ns, ds.priors$ns)
  to.add <- data.table::CJ(ns=missing, lu.to=unique(ds.priors$lu.to))
  to.add$value <- ifelse(to.add$lu.to=="OthAgr",1,0)
  ds.priors <- as.data.frame(data.table::rbindlist(list(ds.priors, to.add)))
}


##### bugfix, make priors sum to 1
ds.priors <- ds.priors %>% group_by(ns) %>% mutate(value=value/sum(value))


##### BUGFIX, remove OthAgri and ad the remaining areas after again

ds.targets <- ds.targets %>% filter(lu.to!="OthAgr")

test <- downscale(ds.targets, ds.start.areas, xmat = NULL,betas = NULL ,priors = ds.priors)
test$out.res <- as.data.table(test$out.res)

##### BUGFIX, remove OthAgri and ad the remaining areas after again
residual_rows <- test$out.res %>%
  group_by(times, ns) %>%
  summarise(sum_val = sum(value, na.rm = TRUE), .groups = "drop") %>%
  inner_join(ds.start.areas, by = "ns") %>%
  mutate(
    lu.to = "OthAgr",
    value = value - sum_val # 'value' from ds.start.areas minus 'sum_val'
  ) %>%
  select(times, ns, lu.to, value)

test$out.res <- bind_rows(test$out.res, residual_rows)




# Group by `ns` and `lu.to`, then sum the `value`
result <- test$out.res[, .(sum_value = sum(value)), by = .(ns, lu.to)]
result.table <- dcast(
  result,
  ns ~ lu.to,
  value.var = "sum_value",
  fun.aggregate = sum # Handle duplicate entries by summing values
)

other.agri.res <- result.table %>% dplyr::select(ns, OthAgr)

final.alloc <- other.agri.res %>%
  # Rename to match target structure
  rename(CELLCODE = ns, value = OthAgr) %>%

  # Add fixed or placeholder columns
  mutate(
    LUM.class = "OthAgr",
    lu.to     = "OthAgr",
    irrigation = NA_real_,
    area = NA_real_
  ) %>%

  # Reorder columns
  select(CELLCODE, LUM.class, lu.to, value, irrigation, area)


# Calculate column sums for all numeric columns
col_sums_DS <- result.table[, lapply(.SD, sum), .SDcols = is.numeric]

