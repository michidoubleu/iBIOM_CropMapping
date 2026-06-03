# 1. Install renv (if not already installed)
if (!require("renv")) install.packages("renv")

# 2. Initialize renv (if it's not initialized yet)
if (!file.exists("renv.lock")) {
  cat("Initializing renv...\n")
  renv::init()
} else {
  cat("renv already initialized, skipping init.\n")
}

# 3. Activate the project (necessary for snapshot)
renv::activate()

