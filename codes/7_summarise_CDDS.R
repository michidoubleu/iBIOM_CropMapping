library(stringr)
library(withr)
require(progress)
library(tidyr)
library(dplyr)
##################################################################################

res <- list.files(paste0("output/",RESULT_TAG,"/"), full.names = T)

res <- res[!grepl("FAILED_", res)]
res <- res[grepl("rds", res)]

compare.res <- NULL
full.res <- NULL
irri.res <- NULL
rrr <- res[21]
for (rrr in res) {
  tryCatch({
    to.save <- readRDS(rrr)  # Assumes this loads 'to.save'

    compare.res <- bind_rows(compare.res, to.save[[2]])
    irri.res <- bind_rows(irri.res, to.save[[4]] %>% mutate(region=unique(to.save[[2]]$region),year=unique(to.save[[2]]$year)))
    full.res <- bind_rows(full.res, to.save[[1]]) %>%
      dplyr::select(-any_of(c("area", "irrigation")))

  }, error = function(e) {
    message("Skipping file due to error: ", rrr)
    # Optionally: message(e$message)
  })
}

agg.res <- list(compare=compare.res, res=full.res)
check <- agg.res$res %>% filter(value>0, lu.to!="OthAgr")
save(agg.res, file=paste0("output/",RESULT_TAG,"/aggregated/",RESULT_TAG,"_CDDS.RData"))


check <- check %>% filter(is.na(YLD))







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

# Stack the plots
country_plot / crop_plot





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

country_irri_target / crop_irri_target
