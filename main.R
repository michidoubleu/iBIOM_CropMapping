##### CODE MAIN: main file to apply settings and launch the CDDS pipeline (local execution)
#####
##### 01-03  settings, input preparation (support scripts only run if their output is missing), shared inputs
##### 04-12  per country x year: allocation of areas, irrigation, yields and additional variables
##### 13-16  summary over all countries, plots, aggregation to SimU / GLOBIOM LU, run report

#### Author: Michael Wögerer
#### Date: Jan/2025

rm(list = ls())
gc()

## Load user-specific settings (paths, flags, user tag)
source("user_config.R")

## Load libraries
library(dplyr)
library(tidyr)
library(sf)
library(readr)
library(terra)
library(exactextractr)
library(countrycode)
library(data.table)
library(downscalr)
library(stringr)

## Helper functions
source("./codes/support/fun_yield_ordering_check.R")
source("./codes/support/fun_area_balance_check.R")

dir.create("output", showWarnings = FALSE)
dir.create(paste0("output/",RESULT_TAG), showWarnings = FALSE)
dir.create(paste0("output/",RESULT_TAG,"/aggregated"), showWarnings = FALSE)
dir.create(paste0("output/",RESULT_TAG,"/simu_res"), showWarnings = FALSE)

## Settings, input preparation and inputs shared by all countries
source("./codes/01_settings.R")
source("./codes/02_prepare_inputs.R")
source("./codes/03_load_shared_inputs.R")

country_steps <- c(
  "04_initialize_country.R",
  "05_prepare_startmap.R",
  "06_prepare_priors.R",
  "07_load_EPIC.R",
  "08_prepare_targets.R",
  "09_area_matching.R",
  "10_irri_matching.R",
  "11_production_matching.R"
)

if (!exists("SKIP_EXISTING")) SKIP_EXISTING <- FALSE

for (JOB in seq_len(nrow(country_grid))) {

  gc()  # Run garbage collection to free up memory
  job.reg  <- country_grid$lama.regs[JOB]
  job.year <- country_grid$years[JOB]
  res.file  <- paste0("output/",RESULT_TAG,"/", job.reg, "_", job.year, "_",RESULT_TAG,".rds")
  fail.file <- paste0("output/",RESULT_TAG,"/","FAILED_" ,job.reg, "_", job.year, "_",RESULT_TAG,".rds")

  ## the result rds is only written after the country outputs (12), so it marks a complete job
  if (SKIP_EXISTING && file.exists(res.file)) {
    message(sprintf("↷ SKIPPING JOB: %d / %d (Region: %s, Year: %s) - result exists",
            JOB, nrow(country_grid), job.reg, job.year))
    next
  }

  message("\n=======================================================")
  message(sprintf("▶ STARTING JOB: %d / %d (Region: %s, Year: %s)",
          JOB, nrow(country_grid), job.reg, job.year))
  message("=======================================================\n")

  ## ROUTINE STARTS HERE
  tryCatch({
    for (step in country_steps) {
      message(sprintf("  [%s] %s...", substr(step, 1, 2), step))
      source(file.path("./codes", step), local = TRUE)
    }

    message("  [12] 12_country_outputs.R...")
    source("./codes/12_country_outputs.R", local = TRUE)

    message(sprintf("✔ SUCCESS: Job %d completed. Saving results...\n", JOB))

    saveRDS(list(final.alloc, show.res, result, overview.irri), file = res.file)
    if (file.exists(fail.file)) file.remove(fail.file)

  }, error = function(e) {
    message("Error in iteration ", JOB, ": ", conditionMessage(e))
    saveRDS("failed", file = fail.file)
  })
}

## Results over all countries
source("./codes/13_summarise.R")
source("./codes/14_plot_results.R")
source("./codes/15_aggregate_SimU_LU.R")
source("./codes/16_run_report.R")
