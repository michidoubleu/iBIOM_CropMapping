##### F3: Area balance check function
##### Verifies that the final allocation (all crops x management classes + OthAgr)
##### adds up to the initial crop area from the LUM map (script 05), in total and per cell,
##### and that no allocated area is negative.

#### Author: Michael Wögerer
#### Date:   10/2026

#' @param alloc_df    final.alloc: CELLCODE, value (km²)
#' @param start_area  crop.area:   CELLCODE, total_area (km²)
#' @param tol         tolerance in km² for the total and for any single cell
#' @return  Invisibly returns the per-cell comparison table.

check_area_balance <- function(alloc_df, start_area, tol = 1e-6) {

  start <- as.data.table(start_area)[, .(start = sum(total_area)), by = CELLCODE]
  alloc <- as.data.table(alloc_df)[, .(alloc = sum(value, na.rm = TRUE)), by = CELLCODE]

  cmp <- merge(start, alloc, by = "CELLCODE", all = TRUE)
  cmp[is.na(start), start := 0]
  cmp[is.na(alloc), alloc := 0]
  cmp[, diff := alloc - start]

  tot.start <- sum(cmp$start)
  tot.alloc <- sum(cmp$alloc)
  tot.diff  <- tot.alloc - tot.start
  n.cells   <- sum(abs(cmp$diff) > tol)
  n.neg     <- sum(alloc_df$value < -tol, na.rm = TRUE)
  n.na      <- sum(is.na(alloc_df$value))

  message(sprintf("  Area balance: start %.4f km², allocated %.4f km², diff %.2e km² (max cell diff %.2e km²)",
                  tot.start, tot.alloc, tot.diff, max(abs(cmp$diff))))

  problems <- c(
    if (abs(tot.diff) > tol) sprintf("total differs by %.4g km²", tot.diff),
    if (n.cells > 0)         sprintf("%d of %d cells differ by more than %g km²", n.cells, nrow(cmp), tol),
    if (n.neg > 0)           sprintf("%d negative area rows", n.neg),
    if (n.na > 0)            sprintf("%d NA area rows", n.na)
  )

  if (length(problems) > 0) {
    print(as.data.frame(head(cmp[order(-abs(diff))], 10)))
    stop("Area balance check FAILED: ", paste(problems, collapse = "; "))
  }

  message("  ✔ Area balance check PASSED.")
  invisible(cmp)
}
