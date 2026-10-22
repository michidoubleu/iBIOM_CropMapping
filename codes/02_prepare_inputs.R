##### CODE 02: prepare inputs — runs a support script only if its output does not exist yet
##### To force a re-run, delete the output file or list the step in FORCE_PREP (user_config.R),
##### e.g. FORCE_PREP <- c("GLOBIOM_old_crops")

#### Author: Michael Wögerer

if (!exists("FORCE_PREP")) FORCE_PREP <- character()

# in dependency order: step name, support script, outputs it creates
prep_steps <- list(
  list(name = "country_grid",      script = "prep_country_grid.R",      out = country.grid.path),
  list(name = "EPIC_map",          script = "prep_EPIC_map.R",          out = EPIC.map.path),
  list(name = "SimU_CR_LU_map",    script = "prep_SimU_CR_LU_map.R",    out = SimU.map.path),
  list(name = "EPIC_yields",       script = "prep_EPIC_yields.R",       out = EPIC.yields.path),
  list(name = "EPIC_vars",         script = "prep_EPIC_vars.R",         out = EPIC.vars.path),
  list(name = "GLOBIOM_old_crops", script = "prep_GLOBIOM_old_crops.R", out = c(GLOBIOM.yields.path, GLOBIOM.vars.path)),
  if (GLOBIOM_targets)
  list(name = "GLOBIOM_targets",   script = "prep_GLOBIOM_targets.R",   out = GLOBIOM.target.path)
)

for (step in Filter(Negate(is.null), prep_steps)) {
  if (all(file.exists(step$out)) && !step$name %in% FORCE_PREP) next

  message(sprintf("▶ PREPARING %s (support/%s)...", step$name, step$script))
  source(file.path("./codes/support", step$script), local = new.env())
  gc()

  if (!all(file.exists(step$out))) stop("Preparation step '", step$name, "' did not create: ",
                                         paste(step$out[!file.exists(step$out)], collapse = ", "))
}
