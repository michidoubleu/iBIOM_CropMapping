rm(list = ls())
library(stringr)
library(withr)
require(progress)
library(tidyr)
library(dplyr)
##################################################################################
res <- list.files("output/", full.names = T)
res <- res[grepl("CDDS_", res)]
CLUSTER_tag <- as.character(max(substr(res,13,16)))

compare.res <- NULL
full.res <- NULL
rrr <- res[21]
for (rrr in res) {
  tryCatch({
    load(rrr)  # Assumes this loads 'to.save'

    compare.res <- bind_rows(compare.res, to.save[[2]])
    full.res <- bind_rows(full.res, to.save[[1]]) %>%
      dplyr::select(-area, -irrigation)

  }, error = function(e) {
    message("Skipping file due to error: ", rrr)
    # Optionally: message(e$message)
  })
}

agg.res <- list(compare=compare.res, res=full.res)

save(agg.res, file=paste0("output/aggregated/",CLUSTER_tag,"_CDDS.RData"))


