##### CODE 6 (yield-adjustment variant): Production matching via uniform yield scaling
##### Instead of shifting areas between management technologies (LUM classes),
##### this approach scales the EPIC yields uniformly per crop so that total
##### production matches the production target. The adjusted yields are stored
##### in the "YLD" column next to the area ("value") in the final allocation table.

#### Author: auto-generated alternative to 6_production_matching.R
#### Date:   June 2025

# ── 1. Build crop × LUM area table ──────────────────────────────────────────

crop.ares.LUM <- temp.LUM %>% group_by(CELLCODE) %>% mutate(total=sum(total_area), share=total_area/total) %>% dplyr::select(CELLCODE, LUM.class, share) %>% full_join(result %>% rename("CELLCODE"="ns")) %>% mutate(value=share*sum_value) %>% dplyr::select(CELLCODE, LUM.class, lu.to, value) %>%
  mutate(LUM.class = case_when(
    LUM.class %in% lum_crop_map[c(1)] ~ "M1.rf",
    LUM.class %in% lum_crop_map[c(2)] ~ "M2.rf",
    LUM.class %in% lum_crop_map[c(3)] ~ "M3.rf",
    LUM.class %in% lum_crop_map[c(4)] ~ "M4.rf",
    LUM.class %in% lum_crop_map[c(5)] ~ "M5.rf",
    TRUE ~ as.character(LUM.class)
  ))




# Irrigated area by cell and crop
irri.share <- result.irri %>%
  rename(CELLCODE = ns,
         irri.area = irrigated.area) %>%
  select(CELLCODE, lu.to, irri.area)

# Join irrigation and compute irrigation fraction
crop.ares.LUM <- crop.ares.LUM %>%
  left_join(irri.share, by = c("CELLCODE", "lu.to")) %>%
  group_by(CELLCODE, lu.to) %>%
  mutate(
    irri.area = coalesce(first(irri.area), 0),
    total.crop.area = sum(value),
    irri.area = pmin(irri.area, total.crop.area),
    irri.frac = if_else(total.crop.area > 0,
                        irri.area / total.crop.area,
                        0),
    value = value * (1 - irri.frac)
  ) %>%
  ungroup()

# Create one irrigated management row (M5.ir) per cell and crop
crop.ares.ir <- crop.ares.LUM %>%
  group_by(CELLCODE, lu.to) %>%
  summarise(
    value = first(irri.area),
    .groups = "drop"
  ) %>%
  filter(value > 0) %>%
  mutate(LUM.class = "M5.ir")

# Combine rainfed and irrigated management
crop.ares.LUM <- bind_rows(
  crop.ares.LUM %>%
    select(CELLCODE, LUM.class, lu.to, value),
  crop.ares.ir
)














# ── 2. Compute current production per crop and derive adjustment factor ──────

# Join EPIC yields to the area table
crop.ares.with.yld <- crop.ares.LUM %>%
  left_join(final_yields %>% rename("LUM.class" = "Scen", "lu.to" = "GLOBIOM_abr"),
            by = c("CELLCODE", "LUM.class", "lu.to"))

# Current production per crop  (area [km²] * 100 [ha/km²] * yield [t/ha] = tonnes)
current.prod.by.crop <- crop.ares.with.yld %>%
  group_by(lu.to) %>%
  summarise(current.prod = sum(value * 100 * YLD, na.rm = TRUE), .groups = "drop")

# Target production (stored in 1000 t in targets.prod, convert to tonnes)
target.prod <- targets.prod %>% mutate(value = value * 1000)

# ── 3. Compute yield adjustment factor per crop ─────────────────────────────
#     factor = target_production / current_production
#     Yields for every pixel & management class are multiplied by this factor.

yield.adj.factors <- current.prod.by.crop %>%
  rename("GLOBIOM" = "lu.to") %>%
  left_join(target.prod, by = "GLOBIOM") %>%
  mutate(
    adj_factor = ifelse(current.prod > 0, value / current.prod, 1)
  ) %>%
  dplyr::select(GLOBIOM, adj_factor)

message("  Yield adjustment factors:")
for (i in seq_len(nrow(yield.adj.factors))) {
  message(sprintf("    %s : %.4f", yield.adj.factors$GLOBIOM[i], yield.adj.factors$adj_factor[i]))
}

# ── 4. Apply adjustment factor to yields and build final allocation ──────────

final.alloc <- crop.ares.with.yld %>%
  left_join(yield.adj.factors, by = c("lu.to" = "GLOBIOM")) %>%
  mutate(
    YLD = ifelse(!is.na(adj_factor), YLD * adj_factor, YLD)
  ) %>%
  dplyr::select(-adj_factor)

# Append OthAgr rows (from area matching step) — these have no yield
final.alloc <- final.alloc %>%
  bind_rows(
    other.agri.res %>%
      rename(CELLCODE = ns, value = OthAgr) %>%
      mutate(LUM.class = "OthAgr", lu.to = "OthAgr",
             YLD = NA_real_) %>%
      dplyr::select(CELLCODE, LUM.class, lu.to, value, YLD)
  )

final.alloc$region <- lama.reg
final.alloc$year   <- curr.year

# ── 5. TEST: Verify yield ordering  M5 >= M4 >= M3 >= M2 >= M1  per pixel ───────
yield_violations <- test_yield_ordering(final.alloc %>% na.omit())

# ── 6. Diagnostics: area and production summaries ────────────────────────────

final.areas <- final.alloc %>% na.omit() %>%
  group_by(lu.to) %>%
  summarise(final.area = sum(value, na.rm = TRUE)) %>%
  rename("GLOBIOM" = "lu.to") %>%
  left_join(targets.area %>% rename("target.area" = "value"), by = "GLOBIOM") %>%
  mutate(area.diff = final.area - target.area)

final.prod <- final.alloc %>%
  mutate(prod = value * 100 * YLD / 1000) %>%
  group_by(lu.to) %>%
  summarise(final.prod = sum(prod, na.rm = TRUE)) %>%
  rename("GLOBIOM" = "lu.to") %>%
  left_join(targets.prod %>% rename("target.prod" = "value"), by = "GLOBIOM") %>%
  mutate(prod.diff = final.prod - target.prod)

# Determine pass/failed status per crop
failed_crops <- unique(yield_violations$lu.to)

crop_status <- data.frame(GLOBIOM = unique(final.alloc$lu.to)) %>%
  filter(GLOBIOM != "OthAgr") %>%
  mutate(
    yield_ordering_status = ifelse(GLOBIOM %in% failed_crops, "failed", "passed")
  )

show.res <- final.areas %>%
  left_join(final.prod, by = "GLOBIOM") %>%
  left_join(yield.adj.factors, by = "GLOBIOM") %>%
  left_join(crop_status, by = "GLOBIOM")

show.res$region <- lama.reg
show.res$year   <- curr.year

final.targets <- ds.targets %>% rename("area" = "value") %>%
  left_join(targets.prod %>% rename("lu.to" = "GLOBIOM"), by = "lu.to") %>%
  rename("prod" = "value") %>%
  mutate(region = country)
