##### CODE 01: settings — input paths and run options (user-specific settings are in user_config.R)

#### Author: Michael Wögerer

prior.path      <- file.path(INPUT_DIR, "priors/new/")
target.path     <- file.path(INPUT_DIR, "targets/")
EEA.path        <- file.path(INPUT_DIR, "EEA_1km/")
LUM.path        <- file.path(INPUT_DIR, "LUM_data/")
mapping.path    <- file.path(INPUT_DIR, "mappings/")
LUM.energy.path <- file.path(INPUT_DIR, "LUM_fit_with_energy_levels_V5/")
SimU.path       <- file.path(INPUT_DIR, "SIMU_LAEA/5arcmin_simu_world_ETRS_1989_LAEA_wsimuID.shp")
irri.path       <- file.path(INPUT_DIR, "irrigation_Bonn/Total_IR_A_2010.tif")

#### prepared inputs (created by codes/support/prep_*.R via 02_prepare_inputs.R if missing)
country.grid.path   <- "output/country_grid.rds"
EPIC.map.path       <- file.path(INPUT_DIR, "EPIC/MAP/EUEPIC_SimUID_1k_updt.rds")
SimU.map.path       <- file.path(mapping.path, "SimU_CR_LU_map.rds")
EPIC.yields.path    <- file.path(INPUT_DIR, "EPIC/EPIC_yields.rds")
EPIC.vars.path      <- file.path(INPUT_DIR, "EPIC/EPIC_vars.rds")
GLOBIOM.yields.path <- file.path(INPUT_DIR, "EPIC/cropyieldsOLD_GLOBIOM.rds")
GLOBIOM.vars.path   <- file.path(INPUT_DIR, "EPIC/cropvarsOLD_GLOBIOM.rds")
GLOBIOM.target.path <- file.path(target.path, "croptargets_GLOBIOM.rds")

#### additional per-ha variables carried along with the yields (by crop x management class)
#### EPIC where the crop has EPIC data, old GLOBIOM (data_crops.gdx) otherwise
extra_vars <- c("FTN", "IRGA", "OCPD", "dOCPD")  # kg N/ha, mm, t C/ha, t C/ha/yr
#### variables multiplied by the same production adjustment factor as YLD (script 11);
#### the SOC stock (OCPD) is not scaled, a soil C stock does not depend on the yield bias
scale_vars <- c("FTN", "IRGA", "dOCPD")

#### crops without own EPIC data take these variables from the EPIC data of the most similar crop
#### (same cell and management class), see codes/support/crop_crosswalk_EPIC.csv and README;
#### variables not listed here keep the crop-specific old GLOBIOM value (FTN, IRGA)
crosswalk.path <- "./codes/support/crop_crosswalk_EPIC.csv"
crosswalk_vars <- c("OCPD", "dOCPD")
