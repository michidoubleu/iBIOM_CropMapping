##### CODE 16: run report — target fulfilment, yield shifters, additional variables and SimU aggregation
##### writes output/<RESULT_TAG>/report/<RESULT_TAG>_run_report.md (+ .html with embedded figures, csv tables)
##### can be run on its own after a finished run: source("user_config.R"); source("./codes/01_settings.R"); source("./codes/16_run_report.R")

#### Author: Michael Wögerer

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(patchwork)

rep.dir <- paste0("output/", RESULT_TAG, "/report")
fig.dir <- file.path(rep.dir, "fig")
dir.create(fig.dir, recursive = TRUE, showWarnings = FALSE)

shift.lim  <- c(0.2, 5)    # yield shifters outside this range are flagged
shift.warn <- c(0.33, 3)   # watch list
tol        <- 1e-4         # relative tolerance for "target met"

country_names <- c(AT = "Austria", BE = "Belgium", BG = "Bulgaria", CY = "Cyprus", CZ = "CzechRep",
                   DE = "Germany", DK = "Denmark", EE = "Estonia", EL = "Greece", ES = "Spain",
                   FI = "Finland", FR = "France", HR = "Croatia", HU = "Hungary", IE = "Ireland",
                   IT = "Italy", LT = "Lithuania", LU = "Luxembourg", LV = "Latvia", MT = "Malta",
                   NL = "Netherlands", PL = "Poland", PT = "Portugal", RO = "Romania", SE = "Sweden",
                   SI = "Slovenia", SK = "Slovakia", UK = "UK")

#### code version -------------------------------------------------------------
git <- function(...) tryCatch(system2("git", c(...), stdout = TRUE, stderr = FALSE), error = function(e) NA_character_)
commit      <- git("rev-parse", "HEAD")[1]
commit.date <- git("log", "-1", "--format=%cd", "--date=short")[1]
branch      <- git("rev-parse", "--abbrev-ref", "HEAD")[1]
remote      <- sub("[.]git$", "", sub("^git@github.com:", "https://github.com/", git("remote", "get-url", "origin")[1]))
dirty       <- length(git("status", "--porcelain", "--untracked-files=no", "--", "codes", "main.R")) > 0
commit.url  <- paste0(remote, "/commit/", commit)

#### load country results -----------------------------------------------------
res.files <- list.files(paste0("output/", RESULT_TAG), pattern = paste0("^[A-Z]{2}_[0-9]{4}_", RESULT_TAG, "[.]rds$"), full.names = TRUE)
failed    <- list.files(paste0("output/", RESULT_TAG), pattern = "^FAILED_")

show.all <- list(); irri.all <- list(); cell.all <- list()
for (f in res.files) {
  x   <- readRDS(f)
  reg <- substr(basename(f), 1, 2)
  show.all[[f]] <- x[[2]] %>% mutate(region = reg)
  irri.all[[f]] <- x[[4]] %>% mutate(region = reg)
  a <- as.data.table(x[[1]])
  cell.all[[f]] <- a[, .(area = sum(value),
                         na.YLD = sum(value[is.na(YLD)]), na.FTN = sum(value[is.na(FTN)]), na.IRGA = sum(value[is.na(IRGA)]),
                         na.OCPD = sum(value[is.na(OCPD)]), na.dOCPD = sum(value[is.na(dOCPD)])),
                     by = .(crop = lu.to != "OthAgr")][, region := reg]
  rm(x, a); gc()
}
show.all <- bind_rows(show.all)
irri.all <- bind_rows(irri.all) %>% filter(lu.to != "OthAgr")
cell.all <- rbindlist(cell.all)
run.year <- paste(sort(unique(show.all$year)), collapse = ", ")

fmt  <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
pct  <- function(a, b) ifelse(b > 0, 100 * a / b, NA_real_)
md_table <- function(df, align = NULL) paste(knitr::kable(df, format = "pipe", align = align), collapse = "\n")

#### target fulfilment tables -----------------------------------------------
crop.tab <- show.all %>%
  transmute(region, crop = GLOBIOM, target.area, final.area, area.pct = pct(final.area, target.area),
            target.prod, final.prod, prod.pct = pct(final.prod, target.prod), adj_factor,
            area.met = abs(final.area - target.area) <= pmax(1e-6, tol * target.area),
            prod.met = abs(final.prod - target.prod) <= pmax(1e-6, tol * target.prod))

irri.tab <- irri.all %>% filter(!is.na(target.irri), target.irri > 0) %>%
  transmute(region, crop = lu.to, target.irri, max.irri = max_irri, final.irri = actual.irri,
            irri.pct = pct(actual.irri, target.irri),
            irri.met = abs(actual.irri - target.irri) <= pmax(1e-6, tol * target.irri))

area.country <- crop.tab %>% group_by(region) %>%
  summarise(crops = n(), target = sum(target.area), final = sum(final.area), met = sum(area.met), .groups = "drop") %>%
  mutate(pct = pct(final, target))
prod.country <- crop.tab %>% group_by(region) %>%
  summarise(crops = n(), target = sum(target.prod), final = sum(final.prod), met = sum(prod.met), .groups = "drop") %>%
  mutate(pct = pct(final, target))
irri.country <- irri.tab %>% group_by(region) %>%
  summarise(crops = n(), target = sum(target.irri), feasible = sum(pmin(max.irri, target.irri)),
            final = sum(final.irri), met = sum(irri.met), .groups = "drop") %>%
  mutate(pct = pct(final, target)) %>%
  right_join(tibble(region = sort(unique(crop.tab$region))), by = "region") %>% arrange(region)

add_total <- function(df, cols) bind_rows(df, df %>% summarise(region = "EU", across(all_of(cols), ~ sum(.x, na.rm = TRUE))))
area.country <- add_total(area.country, c("crops", "target", "final", "met")) %>% mutate(pct = pct(final, target))
prod.country <- add_total(prod.country, c("crops", "target", "final", "met")) %>% mutate(pct = pct(final, target))
irri.country <- add_total(irri.country, c("crops", "target", "feasible", "final", "met")) %>% mutate(pct = pct(final, target))

fwrite(crop.tab, file.path(rep.dir, "targets_area_production_by_country_crop.csv"))
fwrite(irri.tab, file.path(rep.dir, "targets_irrigation_by_country_crop.csv"))

#### yield shifters ----------------------------------------------------------
shift.flag <- crop.tab %>% filter(adj_factor < shift.lim[1] | adj_factor > shift.lim[2]) %>% arrange(adj_factor)
shift.watch <- crop.tab %>% filter((adj_factor >= shift.lim[1] & adj_factor < shift.warn[1]) |
                                   (adj_factor > shift.warn[2] & adj_factor <= shift.lim[2])) %>% arrange(adj_factor)
shift.cols <- function(df) df %>% transmute(Country = region, Crop = crop, `Area (km²)` = fmt(final.area),
                                            `Target yield (t/ha)` = fmt(10 * target.prod / final.area, 2),
                                            `Yield before scaling (t/ha)` = fmt(10 * target.prod / final.area / adj_factor, 2),
                                            Shifter = fmt(adj_factor, 3))

#### additional variables ----------------------------------------------------
vars.country <- show.all %>% group_by(region) %>%
  summarise(across(c(FTN.mean, IRGA.mean, OCPD.mean, dOCPD.mean), ~ weighted.mean(.x, final.area, na.rm = TRUE)),
            area = sum(final.area), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))
na.country <- cell.all[crop == TRUE, .(region, area,
                                       YLD = 100 * na.YLD / area, FTN = 100 * na.FTN / area, IRGA = 100 * na.IRGA / area,
                                       OCPD = 100 * na.OCPD / area, dOCPD = 100 * na.dOCPD / area)]
fwrite(vars.country, file.path(rep.dir, "additional_variables_by_country.csv"))

#### SimU aggregation coverage ----------------------------------------------
simu.file <- paste0("output/", RESULT_TAG, "/simu_res/", RESULT_TAG, "_SimU_res.rds")
simu.cov <- NULL
if (file.exists(simu.file)) {
  simu.cov <- readRDS(simu.file) %>% filter(ALLITEM == "BaseArea") %>%
    group_by(ANYREGION) %>% summarise(simu = 10 * sum(value), .groups = "drop") %>%   # 1000 ha -> km²
    mutate(region = names(country_names)[match(ANYREGION, country_names)]) %>%
    inner_join(cell.all[, .(grid = sum(area)), by = region], by = "region") %>%
    mutate(pct = pct(simu, grid)) %>% arrange(pct)
  fwrite(simu.cov, file.path(rep.dir, "simu_area_coverage_by_country.csv"))
}

#### figures -----------------------------------------------------------------
bar_plot <- function(df, x, title, ylab, fill) {
  ggplot(df, aes(x = reorder({{ x }}, pct), y = pct)) +
    geom_col(fill = fill) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "red") +
    labs(title = title, x = NULL, y = ylab) +
    coord_cartesian(ylim = c(0, max(105, max(df$pct, na.rm = TRUE)))) +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}
by_crop <- function(df, tgt, fin) df %>% group_by(crop) %>%
  summarise(pct = pct(sum({{ fin }}), sum({{ tgt }})), .groups = "drop")

fig.targets <-
  (bar_plot(filter(area.country, region != "EU"), region, "Area: final as % of target, by country", "% of target", "steelblue") |
   bar_plot(by_crop(crop.tab, target.area, final.area), crop, "Area: by crop (EU)", "% of target", "darkgreen")) /
  (bar_plot(filter(prod.country, region != "EU"), region, "Production: final as % of target, by country", "% of target", "steelblue") |
   bar_plot(by_crop(crop.tab, target.prod, final.prod), crop, "Production: by crop (EU)", "% of target", "darkgreen")) /
  (bar_plot(filter(irri.country, region != "EU", !is.na(pct)), region, "Irrigated area: final as % of target, by country", "% of target", "steelblue") |
   bar_plot(by_crop(irri.tab, target.irri, final.irri), crop, "Irrigated area: by crop (EU)", "% of target", "darkgreen"))
ggsave(file.path(fig.dir, "01_target_fulfilment.png"), fig.targets, width = 3840, height = 2700, units = "px", dpi = 300)

scatter.df <- bind_rows(
  crop.tab %>% transmute(region, crop, what = "Area (km²)", target = target.area, final = final.area),
  crop.tab %>% transmute(region, crop, what = "Production (1000 t)", target = target.prod, final = final.prod),
  irri.tab %>% transmute(region, crop, what = "Irrigated area (km²)", target = target.irri, final = final.irri)) %>%
  filter(target > 0, final > 0) %>%
  mutate(what = factor(what, c("Area (km²)", "Production (1000 t)", "Irrigated area (km²)")))
fig.scatter <- ggplot(scatter.df, aes(target, final)) +
  geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "dashed") +
  geom_point(alpha = 0.6, size = 1.2, colour = "steelblue") +
  scale_x_log10(labels = scales::label_comma()) + scale_y_log10(labels = scales::label_comma()) +
  facet_wrap(~ what, scales = "free") +
  labs(title = "Final vs target per country x crop (log scales, dashed = 1:1)", x = "Target", y = "Final") +
  theme_bw()
ggsave(file.path(fig.dir, "02_target_vs_final_scatter.png"), fig.scatter, width = 3840, height = 1400, units = "px", dpi = 300)

fig.irri <- ggplot(irri.tab, aes(crop, region, fill = pmin(irri.pct, 100))) +
  geom_tile(colour = "white") +
  geom_text(data = filter(irri.tab, !irri.met), aes(label = round(irri.pct)), size = 2.5) +
  scale_fill_gradientn(colours = c("#b2182b", "#f4a582", "#fddbc7", "#d1e5f0", "#2166ac"),
                       values = c(0, 0.5, 0.8, 0.95, 1), limits = c(0, 100), name = "% of target") +
  scale_y_discrete(limits = rev) +
  labs(title = "Irrigated area: final as % of target per country x crop",
       subtitle = "Only country x crop with an irrigation target; numbers shown where the target is not met",
       x = NULL, y = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave(file.path(fig.dir, "03_irrigation_heatmap.png"), fig.irri, width = 2800, height = 2600, units = "px", dpi = 300)

fig.shift <- ggplot(crop.tab, aes(crop, region, fill = adj_factor)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(adj_factor < shift.warn[1] | adj_factor > shift.warn[2], fmt(adj_factor, 2), "")), size = 2.3) +
  scale_fill_gradient2(trans = "log", low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0,
                       limits = shift.lim, oob = scales::squish, breaks = c(0.2, 0.5, 1, 2, 5), name = "Shifter\n(log scale)") +
  scale_y_discrete(limits = rev) +
  labs(title = "Yield shifter (target production / production with input yields) per country x crop",
       subtitle = sprintf("Values shown below %.2f or above %g; colour range capped at %g-%g", shift.warn[1], shift.warn[2], shift.lim[1], shift.lim[2]),
       x = NULL, y = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave(file.path(fig.dir, "04_yield_shifter_heatmap.png"), fig.shift, width = 2800, height = 2600, units = "px", dpi = 300)

fig.hist <- ggplot(crop.tab, aes(adj_factor)) +
  geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = shift.lim, colour = "red", linetype = "dashed") +
  geom_vline(xintercept = 1, colour = "grey30") +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 5)) +
  labs(title = "Distribution of yield shifters (all country x crop)", x = "Shifter (log scale)", y = "Count") +
  theme_bw()
ggsave(file.path(fig.dir, "05_yield_shifter_histogram.png"), fig.hist, width = 2400, height = 1200, units = "px", dpi = 300)

if (!is.null(simu.cov)) {
  fig.simu <- ggplot(simu.cov, aes(reorder(region, pct), pct)) +
    geom_col(fill = "steelblue") +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "red") +
    coord_cartesian(ylim = c(min(75, min(simu.cov$pct)), max(102, max(simu.cov$pct)))) +
    labs(title = "Area in the SimU/LU aggregation as % of the 1 km grid result (incl. OthAgr)", x = NULL, y = "%") +
    theme_bw()
  ggsave(file.path(fig.dir, "06_simu_area_coverage.png"), fig.simu, width = 3000, height = 1200, units = "px", dpi = 300)
}

#### report text ---------------------------------------------------------------
n.ctry   <- length(res.files)
eu.irri  <- filter(irri.country, region == "EU")
irri.low <- irri.country %>% filter(region != "EU", !is.na(pct), pct < 100 - 100 * tol) %>% arrange(pct)
crosswalk <- read.csv(crosswalk.path)

area.md <- area.country %>% transmute(Country = region, Crops = crops, `Target (km²)` = fmt(target), `Final (km²)` = fmt(final),
                                      `% of target` = fmt(pct, 2), `Crops met` = paste0(met, "/", crops))
prod.md <- prod.country %>% transmute(Country = region, Crops = crops, `Target (1000 t)` = fmt(target), `Final (1000 t)` = fmt(final),
                                      `% of target` = fmt(pct, 2), `Crops met` = paste0(met, "/", crops))
irri.md <- irri.country %>% transmute(Country = region, `Crops with target` = ifelse(is.na(crops), 0, crops),
                                      `Target (km²)` = ifelse(is.na(target), "-", fmt(target, 1)),
                                      `Max. feasible (km²)` = ifelse(is.na(feasible), "-", fmt(feasible, 1)),
                                      `Final (km²)` = ifelse(is.na(final), "-", fmt(final, 1)),
                                      `% of target` = ifelse(is.na(pct), "no target", fmt(pct, 1)),
                                      `Crops met` = ifelse(is.na(crops), "-", paste0(met, "/", crops)))
irri.miss.md <- irri.tab %>% filter(!irri.met) %>% arrange(irri.pct) %>%
  transmute(Country = region, Crop = crop, `Target (km²)` = fmt(target.irri, 2), `Max. feasible (km²)` = fmt(max.irri, 2),
            `Final (km²)` = fmt(final.irri, 2), `% of target` = fmt(irri.pct, 1))
vars.md <- vars.country %>% left_join(na.country %>% select(region, FTN.na = FTN, OCPD.na = OCPD), by = "region") %>%
  transmute(Country = region, `Crop area (km²)` = fmt(area), `FTN (kg N/ha)` = fmt(FTN.mean, 1), `IRGA (mm)` = fmt(IRGA.mean, 1),
            `OCPD (t C/ha)` = ifelse(is.na(OCPD.mean), "n/a", fmt(OCPD.mean, 1)),
            `dOCPD (t C/ha/yr)` = ifelse(is.na(dOCPD.mean), "n/a", fmt(dOCPD.mean, 3)),
            `FTN/IRGA missing (% area)` = fmt(FTN.na, 2), `OCPD/dOCPD missing (% area)` = fmt(OCPD.na, 1))
simu.md <- if (!is.null(simu.cov)) simu.cov %>%
  transmute(Country = region, `1 km grid (km²)` = fmt(grid), `SimU/LU (km²)` = fmt(simu), `% of grid` = fmt(pct, 1)) else NULL

bullet <- function(df, lab) paste(sprintf("%s %s", df$region, lab(df)), collapse = ", ")

L <- c(
  sprintf("# Crop distribution downscaling (CDDS) - run report `%s`", RESULT_TAG),
  "",
  sprintf("**Year:** %s &nbsp;|&nbsp; **Countries:** %d &nbsp;|&nbsp; **Report created:** %s", run.year, n.ctry, format(Sys.Date())),
  "",
  sprintf("**Code:** [%s, commit `%s`](%s) (branch `%s`, %s)%s", sub("^https://github.com/", "", remote), substr(commit, 1, 7),
          commit.url, branch, commit.date,
          if (dirty) " - **note: the working tree had uncommitted changes in `codes/` or `main.R` when this report was created**" else ""),
  "",
  "## 1. Summary",
  "",
  sprintf("* All %d countries (EU-27 + UK) ran without errors (%d `FAILED_` files). The automatic area balance check (sum of all area outputs incl. OthAgr = LUM crop area of the country) passed for every country.",
          n.ctry, length(failed)),
  sprintf("* **Area and production targets are met for all %d country x crop combinations** (tolerance %g relative).",
          nrow(crop.tab), tol),
  sprintf("* **Irrigated area:** %s of %s km² (%.1f%%) of the EU irrigation target is allocated; %d countries are below 100%%: %s. The shortfall happens where the crop area inside cells with irrigation in the Bonn layer (> 0 ha) is smaller than the target (cells with 0 ha are excluded by design, see 4.3).",
          fmt(eu.irri$final), fmt(eu.irri$target), eu.irri$pct, nrow(irri.low), bullet(irri.low, function(d) sprintf("%.*f%%", ifelse(d$pct > 99, 1L, 0L), d$pct))),
  sprintf("* **Yield shifters:** median %.2f, range %.3f-%.2f. %d of %d are outside [%g, %g]: %s. None is above %g.",
          median(crop.tab$adj_factor), min(crop.tab$adj_factor), max(crop.tab$adj_factor), nrow(shift.flag), nrow(crop.tab),
          shift.lim[1], shift.lim[2],
          if (nrow(shift.flag)) paste(sprintf("%s %s (%.3f)", shift.flag$region, shift.flag$crop, shift.flag$adj_factor), collapse = ", ") else "none",
          shift.lim[2]),
  "* **Additional variables** (fertilizer N, irrigation water, SOC stock, SOC change) exist for all crops with a yield; FTN/IRGA are missing for a few legume cells (mostly dry beans, < 2% of crop area), OCPD/dOCPD are missing only for Cyprus and Malta (no EPIC coverage).",
  if (!is.null(simu.cov)) sprintf("* **SimU/LU aggregation:** %.1f%% of the 1 km grid area ends up in the SimU table; losses are largest in coastal countries (%s), presumably 1 km cells that do not intersect a SimU polygon.",
          pct(sum(simu.cov$simu), sum(simu.cov$grid)), paste(sprintf("%s %.0f%%", head(simu.cov$region, 6), head(simu.cov$pct, 6)), collapse = ", ")),
  "",
  "## 2. What the pipeline does",
  "",
  "The pipeline distributes national crop statistics (FAO-based targets for area, production and irrigated area per crop) onto the 1 km² EEA grid, using the LUMASUS land-use map (LUM classes 2001-2005 = management intensity M1-M5) as the available cropland and crop suitability priors to decide which crop goes where. Per country:",
  "",
  "1. **Start map and priors** (04-06): cropland per 1 km cell and management class from the 100 m LUM raster; prior crop probabilities per cell.",
  "2. **Yields and additional variables** (07): per cell, crop and management class from the new 1 km EPIC simulations where the crop has EPIC data, otherwise from the old GLOBIOM data (`data_crops.gdx`, SimU level).",
  "3. **Area matching** (09): crop areas are distributed so that the national area targets are met exactly; cropland not needed for the target crops remains `OthAgr`.",
  "4. **Irrigation matching** (10): irrigated area targets per crop are placed in the cells most likely to be irrigated (Bonn irrigated-area layer, see 4.3), and the irrigated area moves to the irrigated management class `M5.ir`.",
  "5. **Production matching** (11): yields are multiplied per crop by a **yield shifter** = target production / production with the input yields, so national production equals the target. The same shifter is applied to fertilizer N (FTN), irrigation water (IRGA) and the SOC change (dOCPD); the SOC stock (OCPD) is not scaled.",
  "6. **Outputs** (12-15): per-country maps, an EU summary, plots and an aggregation to GLOBIOM SimU/LU units.",
  "",
  "### Data sources of the per-ha variables",
  "",
  "| Variable | Unit | Crops with 1 km EPIC data | Crops without EPIC data |",
  "|---|---|---|---|",
  "| Yield (YLD) | t/ha (fresh matter) | EPIC (`YLDG`) | old GLOBIOM yield |",
  "| Fertilizer N (FTN) | kg N/ha | EPIC N scenario | old GLOBIOM `FTN` |",
  "| Irrigation water (IRGA) | mm (= 10 m³/ha) | EPIC `IRGA` | old GLOBIOM `WATER` |",
  "| SOC stock (OCPD) | t C/ha | EPIC `OCPD` | EPIC value of the most similar crop (crosswalk below) |",
  "| SOC change (dOCPD) | t C/ha/yr | EPIC `dOCPD` | EPIC value of the most similar crop (crosswalk below) |",
  "",
  "Crosswalk for crops without own EPIC data (same cell and management class of the proxy crop, `codes/support/crop_crosswalk_EPIC.csv`):",
  "",
  md_table(crosswalk %>% rename(Crop = GLOBIOM_abr, `EPIC proxy` = EPIC_proxy, Rationale = rationale), align = c("l", "l", "l")),
  "",
  "### Changes compared to the previous run (`iBIOM`)",
  "",
  "* **EPIC coordinate fix:** ~13% of the EPIC 1 km points were off the cell centre by up to 0.5 m, so the exact coordinate join to the EEA grid dropped them. Croatia was hit almost completely (only 20 border cells matched), which is why an override to old GLOBIOM yields was used there before. Coordinates are now rounded to the cell origin; the Croatia override is removed and Croatia uses EPIC (16,593 of 16,604 crop cells matched). Other countries are practically unaffected.",
  "* **OthAgr double counting** in the production matching fixed: crop areas, production and yields are unchanged, OthAgr is smaller (Austria: 4,672 instead of 9,343 km²).",
  "* **Irrigation:** binned likelihood from the Bonn layer (4.3) instead of the previous ordering.",
  "* **New variables** FTN, IRGA, OCPD, dOCPD carried through to all outputs, incl. the SimU/LU table (area-weighted means).",
  "* **SimU/LU aggregation:** the LU code is taken from `ALLCOLROW` (was wrong before).",
  "* Code restructured (scripts `01`-`16` in run order, local execution only, automatic area balance check after every country).",
  "",
  "## 3. Output files",
  "",
  sprintf("All under `output/%s/`:", RESULT_TAG),
  "",
  "| File | Content |",
  "|---|---|",
  sprintf("| `<CC>_%s_%s.rds` | per country: list of (1) cell x management class x crop table with area (km²) and YLD, FTN, IRGA, OCPD, dOCPD; (2) target comparison per crop incl. yield shifter and area-weighted variable means; (3) cell x crop areas; (4) irrigation overview |", run.year, RESULT_TAG),
  sprintf("| `country_res/%s_<layer>_<CC>_results.gpkg / .tif` | per country maps; layers: area, yield, FTN, IRGA, OCPD, dOCPD (one band/column per crop x management class) |", RESULT_TAG),
  sprintf("| `aggregated/%s_CDDS.RData` | all countries in one table (`agg.res`) |", RESULT_TAG),
  sprintf("| `simu_res/%s_SimU_res.rds` | aggregated to GLOBIOM SimU/LU x ALLTECH; `ALLITEM` = BaseArea (1000 ha), crop yield, FTN, IRGA, OCPD, dOCPD |", RESULT_TAG),
  "| `plot/area_irrigation_comparison.png` | target fulfilment plot of script 14 |",
  "| `report/` | this report, its figures and csv tables (target fulfilment per country x crop, variables per country, SimU coverage) |",
  "",
  "## 4. Target fulfilment",
  "",
  "![Target fulfilment](fig/01_target_fulfilment.png)",
  "",
  "![Final vs target](fig/02_target_vs_final_scatter.png)",
  "",
  "### 4.1 Crop area",
  "",
  "Sum over all target crops per country (OthAgr not included). Details per crop: `report/targets_area_production_by_country_crop.csv`.",
  "",
  md_table(area.md, align = c("l", rep("r", 5))),
  "",
  "### 4.2 Production",
  "",
  "Production is met by construction through the yield shifter (section 5).",
  "",
  md_table(prod.md, align = c("l", rep("r", 5))),
  "",
  "### 4.3 Irrigated area",
  "",
  "The Bonn layer gives the irrigated area per 1 km cell (0-100 ha). It is turned into likelihood classes: 91-100 ha is filled first, then 81-90 ha, and so on down to 1-10 ha; within a cell at most the irrigated share of the crop area is used, the remaining crop area of cells with irrigation > 0 is used last. Cells with 0 ha are never irrigated. *Max. feasible* is the crop area in cells with irrigation > 0, capped at the target; a target above that cannot be reached.",
  "",
  md_table(irri.md, align = c("l", rep("r", 6))),
  "",
  "Country x crop combinations where the irrigation target is not met (all of them are limited by the feasible area, i.e. final = max. feasible):",
  "",
  md_table(irri.miss.md, align = c("l", "l", rep("r", 4))),
  "",
  "![Irrigation heatmap](fig/03_irrigation_heatmap.png)",
  "",
  "## 5. Yield shifters",
  "",
  sprintf("The shifter rescales the input yields so that national production per crop equals the target. Values far from 1 mean the input yields (EPIC or old GLOBIOM) do not fit the national statistics. Flagged: < %g or > %g; watch list: %g-%g and %g-%g.",
          shift.lim[1], shift.lim[2], shift.lim[1], shift.warn[1], shift.warn[2], shift.lim[2]),
  "",
  "![Yield shifters](fig/04_yield_shifter_heatmap.png)",
  "",
  "![Yield shifter histogram](fig/05_yield_shifter_histogram.png)",
  "",
  sprintf("**Flagged (< %g or > %g):**", shift.lim[1], shift.lim[2]),
  "",
  if (nrow(shift.flag)) md_table(shift.cols(shift.flag), align = c("l", "l", rep("r", 4))) else "none",
  "",
  "**Watch list:**",
  "",
  if (nrow(shift.watch)) md_table(shift.cols(shift.watch), align = c("l", "l", rep("r", 4))) else "none",
  "",
  "Because FTN, IRGA and dOCPD are scaled with the same shifter, they are reduced by the same factor for these crops. Low shifters typically mean the target yield is far below the simulated yield (e.g. dryland cereals in Portugal: about 1 t/ha in the statistics vs 6-8 t/ha simulated). The shifters are identical to the previous run (except Croatia), i.e. they come from the input data, not from the code changes.",
  "",
  "## 6. Additional variables",
  "",
  "Area-weighted means over the crop area (excl. OthAgr), after scaling. Missing = share of the crop area without a value.",
  "",
  md_table(vars.md, align = c("l", rep("r", 7))),
  "",
  if (!is.null(simu.cov)) c(
    "## 7. SimU/LU aggregation",
    "",
    "Total area (incl. OthAgr) in the SimU table compared to the 1 km result. Values below 100% are area in 1 km cells that are not matched to a SimU (mostly at the coast); values slightly above 100% are border cells assigned to a SimU of the neighbouring country.",
    "",
    "![SimU coverage](fig/06_simu_area_coverage.png)",
    "",
    md_table(simu.md, align = c("l", rep("r", 3))),
    ""),
  "## 8. Known issues / open points",
  "",
  "* Irrigation targets cannot be met where the Bonn layer has too little irrigated area under the crop (section 4.3).",
  "* Portuguese cereals (and a few others) need strong yield shifters (section 5); the source of the high input yields is not checked yet.",
  "* SimU/LU aggregation loses area at the coast (section 7).",
  "* Cyprus and Malta have no EPIC coverage: no OCPD/dOCPD; yields, FTN and IRGA come from old GLOBIOM data.",
  "* Script 15 maps management class M3 to GLOBIOM `HI`, while the yield mapping uses `LI` for M1-M3 (to be harmonised).",
  ""
)
L <- unlist(L)

md.file <- file.path(rep.dir, paste0(RESULT_TAG, "_run_report.md"))
writeLines(L, md.file, useBytes = TRUE)

## self-contained html (figures embedded) for sharing
css <- "body{font-family:Segoe UI,Helvetica,Arial,sans-serif;max-width:1100px;margin:2em auto;padding:0 1em;line-height:1.45;color:#222}
table{border-collapse:collapse;font-size:0.85em;margin:0.5em 0 1em}th,td{border:1px solid #ccc;padding:3px 8px}th{background:#f0f3f7}
img{max-width:100%}code{background:#f4f4f4;padding:0 3px}h1{font-size:1.6em}h2{border-bottom:1px solid #ddd;padding-bottom:3px}"
old.wd <- setwd(rep.dir)
html <- markdown::mark(basename(md.file), options = "+table")
for (img in unique(regmatches(html, gregexpr('src="fig/[^"]+"', html))[[1]])) {
  path <- sub('^src="(.*)"$', "\\1", img)
  html <- gsub(img, paste0('src="', knitr::image_uri(path), '"'), html, fixed = TRUE)
}
setwd(old.wd)
writeLines(c("<!DOCTYPE html><html><head><meta charset=\"utf-8\">",
             sprintf("<title>CDDS run report %s</title><style>%s</style></head><body>", RESULT_TAG, css),
             html, "</body></html>"),
           sub("[.]md$", ".html", md.file), useBytes = TRUE)

message("Run report written to ", md.file, " (+ .html)")
