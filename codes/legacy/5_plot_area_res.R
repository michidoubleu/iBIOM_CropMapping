library(sf)
library(ggplot2)
library(dplyr)



# Join the result data to the spatial object
temp.EEA.forplot <- temp.EEA %>%
  left_join(result, by = c("CELLCODE" = "ns"))

# Filter for a specific `lu.to` value (e.g., "OCER")
lu_to_value <- "Whea"
temp.EEA.filtered <- temp.EEA.forplot %>%
  filter(lu.to == lu_to_value)

# Plot using ggplot
ggplot(data = temp.EEA.filtered) +
  geom_sf(aes(fill = sum_value), color = NA) + # Color polygons by `sum_value`
  scale_fill_viridis_c(option = "viridis", name = paste("Sum Value for", lu_to_value)) +
  theme_minimal() +
  labs(title = paste("Map of", lu_to_value, "Values"), x = "Longitude", y = "Latitude")
