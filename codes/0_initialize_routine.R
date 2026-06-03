##### CODE 0: initialisation for cluster

#### Author: Michael Wögerer
#### Date: Dec/2024

prior.path      <- file.path(INPUT_DIR, "priors/new/")
target.path     <- file.path(INPUT_DIR, "targets/")
EEA.path        <- file.path(INPUT_DIR, "EEA_1km/")
LUM.path        <- file.path(INPUT_DIR, "LUM_data/")
mapping.path    <- file.path(INPUT_DIR, "mappings/")
LUM.energy.path <- file.path(INPUT_DIR, "LUM_fit_with_energy_levels_V5/")
SimU.path       <- file.path(INPUT_DIR, "SIMU_LAEA/5arcmin_simu_world_ETRS_1989_LAEA_wsimuID.shp")
#### cluster setup


  curr.year <- as.character(clustergrid[JOB,"years"])
  country <- as.character(clustergrid[JOB,"countries"])
  lama.reg <- as.character(clustergrid[JOB,"lama.regs"])


##### identify curr prior
prior <- list.files(prior.path, full.names = TRUE)
prior <- prior[grepl(curr.year,prior)]

#### identify curr LUM map
LUM <- list.files(LUM.path, full.names = TRUE)
LUM <- LUM[grepl(curr.year,LUM)]
LUM <- LUM[!grepl(".ovr",LUM)]
LUM <- LUM[!grepl(".qml",LUM)]
LUM <- LUM[!grepl(".aux",LUM)]

#### identify curr LUMenergy map
LUMenergy <- list.files(LUM.energy.path, full.names = TRUE)
LUMenergy <- LUMenergy[grepl(curr.year,LUMenergy)]
LUMenergy <- LUMenergy[!grepl(".ovr",LUMenergy)]
LUMenergy <- LUMenergy[!grepl(".qml",LUMenergy)]
LUMenergy <- LUMenergy[!grepl(".aux",LUMenergy)]

#### identify curr EEA map
EEA.grid <- list.files(EEA.path, full.names = TRUE)
EEA.grid <- EEA.grid[grepl(country,EEA.grid)]
EEA.grid <- list.files(EEA.grid, full.names = TRUE)
EEA.grid <- EEA.grid[grepl(".shp",EEA.grid)]

#### irrigation map
irrig.path <- list.files(file.path(INPUT_DIR, "irrigation/"), full.names = T)
irrig.path <- irrig.path[!grepl(".ovr",irrig.path)]
irrig.path <- irrig.path[!grepl(".qml",irrig.path)]
irrig.path <- irrig.path[!grepl(".aux",irrig.path)]

#### load mapping for country
EEA_NUTS <- read.csv(file.path(mapping.path,"EEAref_LAMAnuts_mapping_oct17.csv"))
length.curr.nuts <- nchar(lama.reg)
curr.EEA_NUTS <- EEA_NUTS %>% filter(substr(NUTS_ID,1,length.curr.nuts)==lama.reg)

### mapping of the priors
crop_mapping <- read.csv(file.path(mapping.path,"CROP_mapping.csv"))
colnames(crop_mapping)[1] <- "CAPRI"
setDT(crop_mapping)
