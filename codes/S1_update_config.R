# Define your config file path
config_path <- "config_CDDS.R"

# Define the current list of jobs
current_jobs <- 1:nrow(clustergrid)

# Read the config file
config_lines <- readLines(config_path)

# Update the JOBS line
config_lines <- gsub(
  pattern = "^JOBS = .*",
  replacement = paste0("JOBS = c(",  paste(current_jobs[1],current_jobs[length(current_jobs)],  sep = ":"), ")"),
  x = config_lines
)

# Write the updated config back to the file
writeLines(config_lines, config_path)