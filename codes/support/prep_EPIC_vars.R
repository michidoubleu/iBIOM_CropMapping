##### SUPPORT: EPIC management-dependent variables prepare
##### Builds one table with all non-yield EPIC variables per SimUID x Scen x CROP,
##### same key as EPIC_yields.rds (prep_EPIC_yields.R):
#####   FTN   : N-fertilization (kg N/ha), parsed from the EPIC N-scenario (e.g. "N250")
#####   IRGA  : annual irrigation volume (mm), only simulated for M5.ir -> 0 for rainfed
#####   OCPD  : initial topsoil organic C stock (t C/ha)   -> SOC
#####   dOCPD : 20-year mean annual change in OCPD (t C/ha/yr) -> carbon flux / emissions

#### Author: Michael Wögerer
#### Date: 10/2026

library(data.table)

read_EPIC_dir <- function(sub) {
  files <- list.files(file.path(INPUT_DIR, "EPIC", sub), full.names = TRUE)
  files <- grep("REF|ReadMe", files, invert = TRUE, value = TRUE)
  rbindlist(lapply(files, fread))
}

OCPD_data <- read_EPIC_dir("OCPD")
IRGA_data <- read_EPIC_dir("IRGA")

EPIC_vars <- OCPD_data[, .(
  SimUID, Scen, CROP,
  FTN   = as.numeric(sub("^N", "", N)),
  OCPD  = OCPD.init,
  dOCPD = dOCPD
)]

EPIC_vars <- merge(EPIC_vars, IRGA_data[, .(SimUID, Scen, CROP, IRGA)],
                   by = c("SimUID", "Scen", "CROP"), all.x = TRUE)

### rainfed management is not irrigated
EPIC_vars[is.na(IRGA) & Scen != "M5.ir", IRGA := 0]

saveRDS(as.data.frame(EPIC_vars), file = EPIC.vars.path)
