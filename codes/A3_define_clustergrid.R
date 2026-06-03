##### CODE 5: define cluster.grid

#### Author: Michael Wögerer
#### Date: 07/02/2025

remove.islands <- c("ES70", "PT20", "PT30", "PT18")
non.LUM <- c("TR", "NO", "MK", "AL")

clustergrid <- readRDS("output/CAPRI_LAMA_NUTSMAP.rds")
colnames(clustergrid) <- c("capri.regs", "lama.regs")

clustergrid$Code <- substr(clustergrid$lama.regs,1,2)


grid.mapping <- data.frame(
  Code = c("ES", "PT", "DE", "EL", "IT", "NL", "HU", "TR", "BG", "NO", "RO", "PL", "CZ", "UK", "FR", "AT", "SE", "SK", "SI", "CY", "DK", "FI", "HR", "LT", "EE", "MK", "MT", "AL", "LV", "IE", "BE"),
  countries = c("Spain", "Portugal", "Germany", "Greece", "Italy", "Netherlands", "Hungary", "Tyrkiye", "Bulgaria",
              "Norway", "Romania", "Poland", "Czechia", "Great-Britain", "France", "Austria", "Sweden", "Slovakia",
              "Slovenia", "Cyprus", "Denmark", "Finland", "Croatia", "Lithuania", "Estonia", "North-Macedonia", "Malta",
              "Albania", "Latvia", "Ireland", "Belgium")
)
clustergrid <- clustergrid %>% filter(!lama.regs %in% remove.islands, !Code%in%non.LUM)

clustergrid <- clustergrid %>% left_join(grid.mapping) %>% dplyr::select(-Code)

years <- c(2018,2010,2000)
clustergrid <- expand_grid(years,clustergrid)

saveRDS(clustergrid, file="output/cluster_cropdist.rds")
