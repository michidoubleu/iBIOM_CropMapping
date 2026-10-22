source("./codes/S2_package_loading.R")

load("input/temp_image.RData")

args <- commandArgs(trailingOnly = TRUE)
JOB <- ifelse(.Platform$GUI == "RStudio",1, as.integer(args[[1]]))
dir.create("output")



  source("./codes/0_initialize_routine.R", local = TRUE)
  cat('I made it after file 0')
  source("./codes/1_prepare_startmap_Linda_LUM.R", local = TRUE)
  cat('I made it after file 1')
  source("./codes/2_prepare_priors_Storm.R", local = TRUE)
  cat('I made it after file 2')
  source("./codes/3_load_EPIC.R", local = TRUE)
  cat('I made it after file 3')
  source("./codes/4_prepare_CAPRI_targets.R", local = TRUE)
  cat('I made it after file 4')
  source("./codes/5_area_matching.R", local = TRUE)
  cat('I made it after file 5')
  source("./codes/6_production_matching.R", local = TRUE)
  cat('I made it after file 6')
  to.save <- list(final.alloc, show.res, result)


save(to.save, file="./output/CDDS.RData")
