##### SUPPORT: define the country x year grid the pipeline loops over

#### Author: Michael Wögerer
#### Date: 07/02/2025

non.LUM <- c("TR", "NO", "MK", "AL")


grid.mapping <- data.frame(
  lama.regs = c("ES", "PT", "DE", "EL", "IT", "NL", "HU", "TR", "BG", "NO", "RO", "PL", "CZ", "UK", "FR", "AT", "SE", "SK", "SI", "CY", "DK", "FI", "HR", "LT", "EE", "MK", "MT", "AL", "LV", "IE", "BE", "LU"),
  countries = c("Spain", "Portugal", "Germany", "Greece", "Italy", "Netherlands", "Hungary", "Tyrkiye", "Bulgaria",
              "Norway", "Romania", "Poland", "Czechia", "Great-Britain", "France", "Austria", "Sweden", "Slovakia",
              "Slovenia", "Cyprus", "Denmark", "Finland", "Croatia", "Lithuania", "Estonia", "North-Macedonia", "Malta",
              "Albania", "Latvia", "Ireland", "Belgium", "Luxembourg")
)

country_grid <- grid.mapping %>% filter(!lama.regs %in% non.LUM)

years <- c(2018,2010,2000)
country_grid <- expand_grid(years,country_grid)

dir.create(dirname(country.grid.path), showWarnings = FALSE)
saveRDS(country_grid, file = country.grid.path)
