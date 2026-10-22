##### CODE 14: plots of area, production and irrigation vs targets
library(dplyr)
library(ggplot2)
library(patchwork)

# Country-level comparison
country_plot <- compare.res %>%
  group_by(region) %>%
  summarise(
    final_area = sum(final.area, na.rm = TRUE),
    target_area = sum(target.area, na.rm = TRUE),
    pct = 100 * final_area / target_area,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(region, pct), y = pct)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "red") +
  labs(
    title = "Final area as % of target area by country",
    x = "Country",
    y = "% of target area"
  ) +
  theme_bw()

# Crop-level comparison
crop_plot <- compare.res %>%
  group_by(GLOBIOM) %>%
  summarise(
    final_area = sum(final.area, na.rm = TRUE),
    target_area = sum(target.area, na.rm = TRUE),
    pct = 100 * final_area / target_area,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(GLOBIOM, pct), y = pct)) +
  geom_col(fill = "darkgreen") +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "red") +
  labs(
    title = "Final area as % of target area by crop",
    x = "Crop",
    y = "% of target area"
  ) +
  theme_bw()




# Country-level comparison
country_plot_prod <- compare.res %>%
  group_by(region) %>%
  summarise(
    final_prod = sum(final.prod, na.rm = TRUE),
    target_prod = sum(target.prod, na.rm = TRUE),
    pct = 100 * final_prod / target_prod,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(region, pct), y = pct)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "red") +
  labs(
    title = "Final production as % of target production by country",
    x = "Country",
    y = "% of target production"
  ) +
  theme_bw()

# Crop-level comparison
crop_plot_prod <- compare.res %>%
  group_by(GLOBIOM) %>%
  summarise(
    final_prod = sum(final.prod, na.rm = TRUE),
    target_prod = sum(target.prod, na.rm = TRUE),
    pct = 100 * final_prod / target_prod,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(GLOBIOM, pct), y = pct)) +
  geom_col(fill = "darkgreen") +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "red") +
  labs(
    title = "Final production as % of target production by crop",
    x = "Crop",
    y = "% of target production"
  ) +
  theme_bw()



#--------------------------------------------------
# 1. Irrigation target capture by country
#--------------------------------------------------

country_irri_target <- irri.res %>%
  filter(lu.to != "OthAgr") %>%
  group_by(region) %>%
  summarise(
    actual_irri = sum(actual.irri, na.rm = TRUE),
    target_irri = sum(target.irri, na.rm = TRUE),
    pct = 100 * actual_irri / target_irri,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(region, pct), y = pct)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "red") +
  labs(
    title = "Actual irrigation as % of target irrigation by country",
    x = "Country",
    y = "% of target irrigation"
  ) +
  theme_bw()

#--------------------------------------------------
# 2. Irrigation target capture by crop
#--------------------------------------------------

crop_irri_target <- irri.res %>%
  filter(lu.to != "OthAgr") %>%
  group_by(lu.to) %>%
  summarise(
    actual_irri = sum(actual.irri, na.rm = TRUE),
    target_irri = sum(target.irri, na.rm = TRUE),
    pct = 100 * actual_irri / target_irri,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(lu.to, pct), y = pct)) +
  geom_col(fill = "darkgreen") +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "red") +
  labs(
    title = "Actual irrigation as % of target irrigation by crop",
    x = "Crop",
    y = "% of target irrigation"
  ) +
  theme_bw()



# Combine into 2x2 layout
combined_plot <- (country_plot | crop_plot) /
  (country_plot_prod | crop_plot_prod) /
  (country_irri_target | crop_irri_target)

# Create plot folder if needed
if (!dir.exists(paste0("output/",RESULT_TAG,"/plot"))) {
  dir.create(paste0("output/",RESULT_TAG,"/plot"))
}

# Save
ggsave(
  filename = paste0("output/",RESULT_TAG,"/plot/area_irrigation_comparison.png"),
  plot = combined_plot,
  width = 3840,
  height = 2160,
  units = "px",
  dpi = 300
)
