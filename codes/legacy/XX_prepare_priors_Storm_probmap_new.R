##### CODE 3: prepare probabilities from Hugo Storm probabilities!

#### Author: Michael Wögerer
#### Date: 08/01/2025


## load 1km² country map
prior.rast <- terra::rast(prior)

## overlay of 1km² country map and 100m LUM map
extraction <- exactextractr::exact_extract(prior.rast, temp.EEA, force_df=T)

names(extraction) <- temp.EEA$CELLCODE
extraction <- bind_rows(extraction, .id = "CELLCODE")
extraction <- as.data.table(extraction)
extraction <- extraction[CELLCODE%in%unique.CELLCODES]

band_names <- c("weight", "BARL", "CITR", "DWHE", "FARA", "FRUI", "GRAS",
  "INDU", "LMAIZ", "OATS", "OCER", "OLIV", "PARI", "POTA", "PULS",
  "LRAPE", "ROOF", "RYEM", "SOYA", "SUGB", "SUNF", "SWHE", "TEXT",
  "TOBA", "VEGE", "VINY")

colnames(extraction)[c(-1,-length(colnames(extraction)))] <- band_names

extraction <- melt(
  extraction,
  id.vars = c("CELLCODE"),
  variable.name = "band_name",
  value.name = "value"
)


setDT(extraction)

weighting <- extraction[band_name=="weight"]
weighting[,value:=value/1000]

# Create a named vector for the mapping: band_name -> prior
band_to_prior <- data.table(GLOBIOM=crop_mapping$GLOBIOM, band_name=crop_mapping$prior)

# Update the band_name column in extraction
extraction <- merge(extraction, band_to_prior, by = "band_name", all.x = TRUE, allow.cartesian=TRUE)
extraction$value <- extraction$value/1000

extraction[,band_name:=NULL]
extraction <- extraction[, .(value = sum(value)), by = c("CELLCODE","GLOBIOM")]
extraction <- na.omit(extraction)

# Reshape the filtered_extraction to wide format
extraction <- dcast(
  extraction,
  CELLCODE ~ GLOBIOM,
  value.var = "value",
  fun.aggregate = sum # Handle duplicate entries by summing values
)

# Add a new column for 1 minus the rowsums of all band columns
extraction[, OthAgr := 1 - rowSums(.SD, na.rm = TRUE), .SDcols = !("CELLCODE")]

extraction[OthAgr < 0, OthAgr := 0]
numeric_cols <- names(extraction)[sapply(extraction, is.numeric)]
# Calculate row sums for numeric columns
extraction[, row_sum := rowSums(.SD), .SDcols = numeric_cols]
# Normalize each numeric column by dividing by the row sum
extraction[, (numeric_cols) := lapply(.SD, function(x) x / row_sum), .SDcols = numeric_cols]
# Remove the temporary row_sum column
extraction[, row_sum := NULL]
save.priors <- extraction


weighting$band_name <- NULL




targets.old <- merge(extraction, weighting, by = "CELLCODE", all.x = TRUE)
# Multiply numeric columns by `total_area`
numeric_cols_temp <- setdiff(names(targets.old)[sapply(targets.old, is.numeric)], c("value"))  # Identify numeric columns except total_area
targets.old[, (numeric_cols_temp) := lapply(.SD, function(x) x * value), .SDcols = numeric_cols_temp]

# Calculate column sums for all numeric columns
col_sums_temp <- targets.old[, lapply(.SD, sum), .SDcols = is.numeric]
