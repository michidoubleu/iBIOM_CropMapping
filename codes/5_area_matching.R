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


##### BUGFIX, as last one ine the namings is somehow missing the target by peanuts
min.target <- ds.targets$lu.to[which(ds.targets$value==min(ds.targets$value[ds.targets$value!=0]))]
new.name <- paste0("ZZ",min.target)

ds.targets$lu.to[ds.targets$lu.to == min.target] <- new.name
ds.priors$lu.to[ds.priors$lu.to == min.target] <- new.name

test <- downscale(ds.targets, ds.start.areas, xmat = NULL,betas = NULL ,priors = ds.priors, options=downscale_control(cutoff=1.0e-8))
test$out.res <- as.data.table(test$out.res)

test$out.res$lu.to[test$out.res$lu.to == new.name] <- min.target
ds.targets$lu.to[ds.targets$lu.to == new.name] <- min.target
ds.priors$lu.to[ds.priors$lu.to == new.name] <- min.target


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

