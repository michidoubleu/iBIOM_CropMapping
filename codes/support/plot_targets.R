# Script to loop over country_grid, load targets, and plot EU country maps facetted by crop

rm(list = ls())
gc()

source("user_config.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(countrycode)

# Ensure giscoR is installed for standard EU country shapes
if (!requireNamespace("giscoR", quietly = TRUE)) {
  install.packages("giscoR", repos = "http://cran.us.r-project.org")
}
library(giscoR)

dir.create("output", showWarnings = FALSE)

# 1. Load country grid (same as main.R)
source("./codes/01_settings.R")
country_grid <- readRDS(country.grid.path)
country_grid <- country_grid %>%
  arrange(desc(years), lama.regs) %>%
  filter(years %in% RUN_YEARS)

# Load crop mappings for FAO
crop_mapping <- read.csv(file.path(INPUT_DIR, "mappings/CROP_mapping.csv"))
colnames(crop_mapping)[1] <- "CAPRI"
fao_to_globiom <- crop_mapping %>%
  distinct(FAO, GLOBIOM) %>%
  filter(!is.na(FAO) & !is.na(GLOBIOM)) %>%
  { setNames(.$GLOBIOM, .$FAO) }

all_targets_area <- data.frame()
all_targets_prod <- data.frame()

# 2. Loop over country_grid to prepare targets
for (JOB in 1:nrow(country_grid)) {
  lama.reg <- country_grid$lama.regs[JOB]
  curr.year <- country_grid$years[JOB]
  country <- countrycode::countrycode(lama.reg, origin = 'iso2c', destination = 'country.name')

  message(sprintf("Processing target for region: %s", lama.reg))

  # Logic matching 4_prepare_targets.R
  if(GLOBIOM_targets) {
    GLOB.targets <- readRDS(GLOBIOM.target.path)
    GLOB.targets <- GLOB.targets %>%
      filter(substr(NUTS_ID, 1, nchar(lama.reg)) == lama.reg) %>%
      group_by(SPECIES) %>%
      summarise(BaseArea = sum(BaseArea, na.rm=TRUE), production = sum(production, na.rm=TRUE))

    targets.area <- GLOB.targets %>%
      ungroup() %>%
      mutate(value = BaseArea / 100, GLOBIOM = SPECIES) %>%
      dplyr::select(GLOBIOM, value)

    targets.prod <- GLOB.targets %>%
      ungroup() %>%
      mutate(value = production / 1000, GLOBIOM = SPECIES) %>%
      dplyr::select(GLOBIOM, value)

  } else {
    FAO_targets <- readRDS(file.path(INPUT_DIR, "targets/faostat_avg_1998_2002.rds"))

    country_map <- c(
      "United Kingdom of Great Britain and Northern Ireland" = "Great-Britain",
      "Netherlands (Kingdom of the)"                         = "Netherlands"
    )
    matches <- FAO_targets$Area %in% names(country_map)
    FAO_targets$Area[matches] <- country_map[FAO_targets$Area[matches]]

    targets.area <- FAO_targets %>%
      filter(Area == country, Element == "Area harvested") %>%
      mutate(
        GLOBIOM = fao_to_globiom[Item],
        value = Mean_Value / 100
      ) %>%
      filter(!is.na(GLOBIOM), !is.na(value)) %>%
      dplyr::select(GLOBIOM, value) %>%
      group_by(GLOBIOM) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup()

    targets.prod <- FAO_targets %>%
      filter(Area == country, Element == "Production") %>%
      mutate(
        GLOBIOM = fao_to_globiom[Item],
        value = Mean_Value / 1000
      ) %>%
      filter(!is.na(GLOBIOM), !is.na(value)) %>%
      dplyr::select(GLOBIOM, value) %>%
      group_by(GLOBIOM) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup()
  }

  if(nrow(targets.area) > 0) {
    targets.area$lama.reg <- lama.reg
    all_targets_area <- rbind(all_targets_area, targets.area)
  }
  if(nrow(targets.prod) > 0) {
    targets.prod$lama.reg <- lama.reg
    all_targets_prod <- rbind(all_targets_prod, targets.prod)
  }
}

# 3. Create EU map and plot
message("Generating facetted plots...")

eu28 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK")

# Get EU map
eu_map <- gisco_get_countries(region = "Europe") %>% filter(CNTR_ID %in% eu28)

# Expand targets to ensure all EU28 countries exist for every crop facet
grid_area <- expand_grid(lama.reg = eu28, GLOBIOM = unique(all_targets_area$GLOBIOM))
all_targets_area_full <- grid_area %>% left_join(all_targets_area, by = c("lama.reg", "GLOBIOM"))
plot_data_area <- eu_map %>% left_join(all_targets_area_full, by = c("CNTR_ID" = "lama.reg"))

grid_prod <- expand_grid(lama.reg = eu28, GLOBIOM = unique(all_targets_prod$GLOBIOM))
all_targets_prod_full <- grid_prod %>% left_join(all_targets_prod, by = c("lama.reg", "GLOBIOM"))
plot_data_prod <- eu_map %>% left_join(all_targets_prod_full, by = c("CNTR_ID" = "lama.reg"))


plot_data_area <- plot_data_area %>%
  mutate(value = ifelse(value < 1, NA, value))

plot_data_prod <- plot_data_prod %>%
  mutate(value = ifelse(value < 1, NA, value))


# Generate Facetted Plot for Area
p_area <- ggplot(plot_data_area) +
  geom_sf(aes(fill = value), color = "gray30", linewidth = 0.2) +
  facet_wrap(~ GLOBIOM, nrow = 3) +
  scale_fill_viridis_b(option = "plasma", name = "Target Area (1000 ha)", trans = "log2",
                       labels = scales::comma, na.value = "grey80", n.breaks = 9) +
  coord_sf(xlim = c(-12, 35), ylim = c(35, 70), expand = FALSE) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(4, "cm"),
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    title = "Regional Crop Area Targets in 1000 ha"
  )

output_file_area <- "output/target_maps_area_facetted.png"
ggsave(output_file_area, plot = p_area, width = 10, height = 8, dpi = 300)
file.copy(output_file_area, "images/", overwrite = TRUE)
message(sprintf("Area plot successfully generated and saved to %s", output_file_area))

# Generate Facetted Plot for Production
p_prod <- ggplot(plot_data_prod) +
  geom_sf(aes(fill = value), color = "gray30", linewidth = 0.2) +
  facet_wrap(~ GLOBIOM, nrow = 3) +
  scale_fill_viridis_b(option = "viridis", name = "Target Prod (t)", trans = "log2",
                       labels = scales::comma, na.value = "grey80", n.breaks = 9) +
  coord_sf(xlim = c(-12, 35), ylim = c(35, 70), expand = FALSE) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(4, "cm"),
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    title = "Regional Crop Production Targets"
  )

output_file_prod <- "output/target_maps_prod_facetted.png"
ggsave(output_file_prod, plot = p_prod, width = 10, height = 8, dpi = 300)
file.copy(output_file_prod, "images/", overwrite = TRUE)
message(sprintf("Production plot successfully generated and saved to %s", output_file_prod))
