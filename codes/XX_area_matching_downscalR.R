##### CODE 4: Area target matching with downscalR

#### Author: Michael Wögerer
#### Date: 08/01/2025

extraction <- merge(
  extraction,   # The long-format data
  crop.area,         # The crop.area table
  by = "CELLCODE",   # Joining on the CELLCODE column
  all.x = TRUE       # Ensures all rows in long_extraction are retained (left join)
)

# Multiply numeric columns by `total_area`
numeric_cols <- setdiff(names(extraction)[sapply(extraction, is.numeric)], c("total_area"))  # Identify numeric columns except total_area
extraction[, (numeric_cols) := lapply(.SD, function(x) x * total_area), .SDcols = numeric_cols]


# Calculate column sums for all numeric columns
col_sums <- extraction[, lapply(.SD, sum), .SDcols = is.numeric]

to.match <-  crop_mapping$prior[match(names(col_sums_temp),crop_mapping$prior)]  # Identify numeric
to.match <- unique(na.omit(to.match))

target.table <- data.frame(band_name=names(col_sums_temp), target=t(col_sums_temp))
target.table <- target.table %>% filter(band_name%in%to.match)

curr.value <- data.frame(band_name=names(col_sums), curr.value=t(col_sums))
curr.value <- curr.value %>% filter(band_name%in%to.match)

target.table <- target.table %>% left_join(curr.value)
target.table$adj.diff <- target.table$target/target.table$curr.value

target.table <- target.table %>% select(band_name,adj.diff)