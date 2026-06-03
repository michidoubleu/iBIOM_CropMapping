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

band_names <- c(
  "weight",
  "Apples and other Fruits",
  "Barley",
  "Citrus Fruits",
  "Durum Wheat",
  "Flowers and ornamental plants",
  "Permanent grassland and meadows",
  "Maize",
  "Rapeseed and turnip",
  "Nurseries",
  "Oats",
  "Other cereals",
  "Other permanent crops",
  "Other forage plants",
  "Other industrial plants",
  "Olive Plantations",
  "Rice",
  "Potatoes",
  "Pulses",
  "Fodder roots and brassicas",
  "Rye",
  "Soya",
  "Sugar beets",
  "Sunflowers",
  "Common wheat and spelt",
  "Other oil-seed or fibre crops",
  "Tobacco",
  "Fresh vegetables, melons, strawberries",
  "Vineyards"
)

colnames(extraction)[c(-1,-length(colnames(extraction)))] <- band_names

extraction <- melt(
  extraction,
  id.vars = c("CELLCODE"),
  variable.name = "band_name",
  value.name = "value"
)


setDT(extraction)


# Create a named vector for the mapping: band_name -> prior
band_to_prior <- setNames(crop_mapping$prior, crop_mapping$band_name)


# Update the band_name column in extraction
extraction[, band_name := ifelse(band_name %in% names(band_to_prior), band_to_prior[band_name], band_name)]
extraction$value <- extraction$value/1000

weigths <- extraction[band_name=="1"]

extraction <- extraction[band_name %in% crop_mapping$prior]




# Reshape the filtered_extraction to wide format
extraction <- dcast(
  extraction,
  CELLCODE ~ band_name,
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


weigths$band_name <- NULL




targets.old <- merge(extraction, weigths, by = "CELLCODE", all.x = TRUE)
# Multiply numeric columns by `total_area`
numeric_cols_temp <- setdiff(names(targets.old)[sapply(targets.old, is.numeric)], c("value"))  # Identify numeric columns except total_area
targets.old[, (numeric_cols_temp) := lapply(.SD, function(x) x * value), .SDcols = numeric_cols_temp]

# Calculate column sums for all numeric columns
col_sums_temp <- targets.old[, lapply(.SD, sum), .SDcols = is.numeric]