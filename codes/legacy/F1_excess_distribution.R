redistribute_excess <- function(df) {
  # Identify rows where actual probability exceeds 1
  excess_rows <- which(df$actual > 1)

  i <- 1
  for (i in excess_rows) {
    excess_actual <- df$to.increase[i] - df$tot.max.change[i]  # Calculate excess in actual proportionally
    # Identify rows that can receive redistribution
    redistributable <- which(df$actual < 1)
    redistributable <- setdiff(redistributable, i)  # Exclude the current excess row

    if (length(redistributable) == 0) {
      warning(paste("Redistribution infeasible for", df$LUM.from[i]))
      df$to.increase[i] <- df$tot.max.change[i]
      next
    }

    # Calculate total max change of eligible classes
    total_max_change <- sum(df$tot.max.change[redistributable])

    if (total_max_change == 0) {
      warning(paste("No valid classes to receive excess from", df$LUM.from[i]))
      next
    }

    # Distribute excess proportionally
    for (j in redistributable) {
      proportion <- df$tot.max.change[j] / total_max_change
      df$to.increase[j] <- df$to.increase[j] + excess_actual * proportion
    }

    df$to.increase[i] <- df$to.increase[i] - excess_actual  # Reduce proportionally
  }#
  df$actual <- df$to.increase/df$tot.max.change
  df$actual[is.infinite(df$actual)] <- 1
  df$actual[is.nan(df$actual)] <- 1
  df$actual[df$actual<0] <- 0

  return(df)
}
redistribute_reduction <- function(df) {
  # Identify rows where actual probability exceeds 1
  excess_rows <- which(df$actual > 1)

  for (i in excess_rows) {
    excess_actual <- df$to.reduce[i] - df$tot.max.change[i]  # Calculate excess in actual proportionally
    # Identify rows that can receive redistribution
    redistributable <- which(df$actual < 1)
    redistributable <- setdiff(redistributable, i)  # Exclude the current excess row

    if (length(redistributable) == 0) {
      warning(paste("Redistribution infeasible for", df$LUM.from[i]))
      df$to.reduce[i] <- df$tot.max.change[i]
      next
    }

    # Calculate total max change of eligible classes
    total_max_change <- sum(df$tot.max.change[redistributable])

    if (total_max_change == 0) {
      warning(paste("No valid classes to receive excess from", df$LUM.from[i]))
      df$to.reduce[i] <- 0
      next
    }

    # Distribute excess proportionally
    for (j in redistributable) {
      proportion <- df$tot.max.change[j] / total_max_change
      df$to.reduce[j] <- df$to.reduce[j] + excess_actual * proportion
    }

    df$to.reduce[i] <- df$to.reduce[i] - excess_actual  # Reduce proportionally
  }#
  df$actual <- df$to.reduce/df$tot.max.change
  df$actual[is.infinite(df$actual)] <- 1
  df$actual[is.nan(df$actual)] <- 1
  df$actual[df$actual<0] <- 0
  return(df)
}