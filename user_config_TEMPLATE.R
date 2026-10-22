##### USER CONFIGURATION — copy this file to user_config.R and edit #####
#### Do NOT commit user_config.R to Git — it is in .gitignore

## Shared data root on network drive (read-only)
INPUT_DIR     <- "P:/bnr/02_Data/_iBIOM/in_progress/crop_distribution_inputs"

## GAMS installation path (only needed for script 15 and the GLOBIOM preparation scripts)
GAMS_PATH     <- "C:/GAMS/49"

## Run identifier (used to tag output files)
RESULT_TAG    <- "res_v1"

## Year(s) to process
RUN_YEARS     <- "2000"

## new FAO targets (FALSE) or old GLOBIOM targets (TRUE)
GLOBIOM_targets <- FALSE

#### Choose which LUM layer ID to distribute targets on
lum_crop_map  <- c(seq(2001,2005,1))

## Preparation steps to re-run even if their output exists (see codes/02_prepare_inputs.R),
## e.g. c("EPIC_vars", "GLOBIOM_old_crops")
FORCE_PREP    <- character()

## Skip countries whose result rds already exists in output/RESULT_TAG (resume an interrupted run)
SKIP_EXISTING <- FALSE
