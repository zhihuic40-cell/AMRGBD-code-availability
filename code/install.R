# install.R - R Package Dependencies
# Install all required packages for the CRGN mortality analysis

cat("========================================\n")
cat("Installing R Package Dependencies\n")
cat("========================================\n\n")

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Don't stop on warnings
options(warn = 1)

# Function to install package with status reporting
# Only installs if package is not already installed
install_package <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("Checking %s... ✓ Already installed (skipping)\n", pkg))
    return(TRUE)
  }
  
  cat(sprintf("Installing %s...\n", pkg))
  tryCatch({
    install.packages(pkg, dependencies = TRUE)
    cat(sprintf("  ✓ %s installed successfully\n", pkg))
    return(TRUE)
  }, error = function(e) {
    cat(sprintf("  ✗ %s installation failed: %s\n", pkg, conditionMessage(e)))
    return(FALSE)
  })
}

# 1. Data manipulation packages
cat("\n1. Installing data manipulation packages...\n")
data_packages <- c(
  "tidyverse",
  "dplyr",
  "tidyr",
  "readr",
  "readxl",
  "data.table"
)

for (pkg in data_packages) {
  install_package(pkg)
}

# 2. Visualization packages
cat("\n2. Installing visualization packages...\n")
viz_packages <- c(
  "ggplot2",
  "patchwork",
  "scales",
  "RColorBrewer",
  "viridis",
  "cowplot",
  "ggpubr",
  "forcats",
  "ggsci"
)

for (pkg in viz_packages) {
  install_package(pkg)
}

# 3. Spatial analysis packages
cat("\n3. Installing spatial analysis packages...\n")
cat("Note: These packages require system libraries (GDAL, GEOS, PROJ, libudunits2)\n")
cat("On Code Ocean, add to Environment -> Packages -> apt-get:\n")
cat("  libgdal-dev libgeos-dev libproj-dev libudunits2-dev\n")
cat("  libcairo2-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev\n")
cat("  libxml2-dev libssl-dev libcurl4-openssl-dev\n")
cat("See CODE_OCEAN_SETUP_GUIDE.md for details\n\n")

# Install sf first (most critical, requires system libraries)
if (requireNamespace("sf", quietly = TRUE)) {
  cat("Checking sf... ✓ Already installed (skipping)\n")
} else {
  cat("Installing sf (Simple Features for R)...\n")
  tryCatch({
    install.packages("sf", dependencies = TRUE)
    cat("  ✓ sf installed successfully\n")
  }, error = function(e) {
    cat("  ✗ sf installation failed\n")
    cat("  Error: ", conditionMessage(e), "\n")
    cat("  CRITICAL: sf requires system libraries!\n")
    cat("  See CODE_OCEAN_SETUP_GUIDE.md for installation instructions\n")
  })
}

# Install rnaturalearth packages (for world map data)
if (requireNamespace("rnaturalearth", quietly = TRUE)) {
  cat("Checking rnaturalearth... ✓ Already installed (skipping)\n")
} else {
  cat("Installing rnaturalearth (Natural Earth map data)...\n")
  tryCatch({
    install.packages("rnaturalearth", dependencies = TRUE)
    cat("  ✓ rnaturalearth installed successfully\n")
  }, error = function(e) {
    cat("  ✗ rnaturalearth installation failed: ", conditionMessage(e), "\n")
  })
}

if (requireNamespace("rnaturalearthdata", quietly = TRUE)) {
  cat("Checking rnaturalearthdata... ✓ Already installed (skipping)\n")
} else {
  cat("Installing rnaturalearthdata...\n")
  tryCatch({
    install.packages("rnaturalearthdata", dependencies = TRUE)
    cat("  ✓ rnaturalearthdata installed successfully\n")
  }, error = function(e) {
    cat("  ✗ rnaturalearthdata installation failed: ", conditionMessage(e), "\n")
  })
}

# Try to install rnaturalearthhires (optional, from r-universe)
if (requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  cat("Checking rnaturalearthhires... ✓ Already installed (skipping)\n")
} else {
  cat("Installing rnaturalearthhires (optional high-res data)...\n")
  tryCatch({
    install.packages("rnaturalearthhires", 
                     repos = "https://ropensci.r-universe.dev",
                     dependencies = TRUE)
    cat("  ✓ rnaturalearthhires installed successfully\n")
  }, error = function(e) {
    cat("  ⚠ rnaturalearthhires installation failed (optional, can skip)\n")
  })
}

# rgeos is deprecated, try to install but don't fail
if (requireNamespace("rgeos", quietly = TRUE)) {
  cat("Checking rgeos... ✓ Already installed (skipping)\n")
} else {
  cat("Installing rgeos (deprecated, optional)...\n")
  tryCatch({
    install.packages("rgeos", dependencies = TRUE)
    cat("  ✓ rgeos installed successfully\n")
  }, error = function(e) {
    cat("  ⚠ rgeos installation failed (OK - deprecated, sf replaces it)\n")
  })
}

# 4. Statistical modeling packages
cat("\n4. Installing statistical modeling packages...\n")
stat_packages <- c(
  "mgcv",
  "nlme",
  "lme4",
  "epitools",
  "reshape2"
)

for (pkg in stat_packages) {
  install_package(pkg)
}

# 5. Additional utilities
cat("\n5. Installing utility packages...\n")
util_packages <- c(
  "here",
  "stringr",
  "lubridate",
  "callr"
)

for (pkg in util_packages) {
  install_package(pkg)
}

# 6. BAPC package (for Figure 6 Bayesian analysis)
cat("\n6. Installing BAPC package from R-Forge...\n")

if (requireNamespace("BAPC", quietly = TRUE)) {
  cat("Checking BAPC... ✓ Already installed (skipping)\n")
} else {
  tryCatch({
    install.packages("BAPC", repos = "http://R-Forge.R-project.org", dependencies = TRUE)
    cat("  ✓ BAPC installed successfully\n")
  }, error = function(e) {
    cat("  ✗ BAPC installation failed\n")
    cat("  Error: ", conditionMessage(e), "\n")
    cat("  Try: Rscript lst.R for special package installation\n")
  })
}

# 7. INLA (for Figure 6 Bayesian inference)
cat("\n7. Installing INLA (Bayesian inference)...\n")

if (requireNamespace("INLA", quietly = TRUE)) {
  cat("Checking INLA... ✓ Already installed (skipping)\n")
} else {
  cat("Note: INLA is a large package and may take several minutes\n")
  
  # Set extended timeout for large download
  old_timeout <- getOption("timeout")
  options(timeout = 10000000)
  
  tryCatch({
    install.packages("INLA",
                     repos = "https://inla.r-inla-download.org/R/stable", 
                     dep = TRUE)
    cat("  ✓ INLA installed successfully\n")
  }, error = function(e) {
    cat("  ✗ INLA installation failed - Figure 6 may not run\n")
    cat("  Error: ", conditionMessage(e), "\n")
    cat("  Try: Rscript lst.R for special package installation\n")
  })
  
  # Restore timeout
  options(timeout = old_timeout)
}

# Check installation results
cat("\n========================================\n")
cat("Checking Installed Packages\n")
cat("========================================\n\n")

# List all critical packages (excluding optional ones like rgeos, rnaturalearthhires)
all_packages <- c(data_packages, viz_packages, 
                  "sf", "rnaturalearth", "rnaturalearthdata",
                  stat_packages, util_packages, "BAPC", "INLA")

installed <- sapply(all_packages, function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
})

cat("\nInstallation Summary:\n")
cat(sprintf("  Successful: %d/%d\n", sum(installed), length(all_packages)))
cat(sprintf("  Failed: %d/%d\n", sum(!installed), length(all_packages)))

if (sum(!installed) > 0) {
  cat("\nFailed packages:\n")
  for (pkg in names(installed)[!installed]) {
    cat(sprintf("  - %s\n", pkg))
  }
  cat("\n")
  cat("Troubleshooting:\n")
  cat("  • sf, rnaturalearth: Install system libraries first\n")
  cat("    - On Ubuntu/Debian: sudo apt install libgdal-dev libgeos-dev libproj-dev libudunits2-dev\n")
  cat("    - On Code Ocean: Add to Environment -> Packages -> apt-get\n")
  cat("  • BAPC: May need R-Forge (repos='http://R-Forge.R-project.org')\n")
  cat("  • INLA: Large download, may need timeout increase (options(timeout=10000000))\n")
  cat("\n  See CODE_OCEAN_SETUP_GUIDE.md for detailed instructions\n")
  cat("\n")
}

cat("\n========================================\n")
cat("Environment Setup Complete!\n")
cat("========================================\n")

# Display R version and system information
cat("\nR Version:\n")
print(R.version.string)
cat("\nSystem Information:\n")
print(Sys.info()[c("sysname", "release", "version")])

cat("\n")
cat("Next steps:\n")
cat("1. Run individual Figure scripts, or\n")
cat("2. Execute run_all.R to run all analyses\n")
cat("\n")
