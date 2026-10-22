##### CODE 13: summarise all country results into one table (agg.res)

library(stringr)
library(withr)
require(progress)
library(tidyr)
library(dplyr)
##################################################################################

res <- list.files(paste0("output/",RESULT_TAG,"/"), full.names = T)

res <- res[!grepl("FAILED_", res)]
res <- res[grepl("rds", res)]

compare.res <- NULL
full.res <- NULL
irri.res <- NULL
rrr <- res[21]
for (rrr in res) {
  tryCatch({
    to.save <- readRDS(rrr)  # Assumes this loads 'to.save'

    compare.res <- bind_rows(compare.res, to.save[[2]])
    irri.res <- bind_rows(irri.res, to.save[[4]] %>% mutate(region=unique(to.save[[2]]$region),year=unique(to.save[[2]]$year)))
    full.res <- bind_rows(full.res, to.save[[1]]) %>%
      dplyr::select(-any_of(c("area", "irrigation")))

  }, error = function(e) {
    message("Skipping file due to error: ", rrr)
    # Optionally: message(e$message)
  })
}

agg.res <- list(compare=compare.res, res=full.res)
check <- agg.res$res %>% filter(value>0, lu.to!="OthAgr")
save(agg.res, file=paste0("output/",RESULT_TAG,"/aggregated/",RESULT_TAG,"_CDDS.RData"))
