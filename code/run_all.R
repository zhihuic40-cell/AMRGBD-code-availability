# Main execution script for Code Ocean
# Runs all Figure analyses sequentially

cat("========================================\n")
cat("Running All Figure Analyses\n")
cat("========================================\n\n")

# Ensure we're in the right directory
# Code Ocean runs from /code, local may run from root
is_code_ocean <- dir.exists("/code") && dir.exists("/data") && dir.exists("/results")

if (is_code_ocean) {
  # On Code Ocean, already in /code directory
  cat("Detected Code Ocean environment\n")
  setwd("/code")
} else if (dir.exists("code") && !dir.exists("Figure1")) {
  # Local execution from root directory
  cat("Detected local root directory, changing to code/\n")
  setwd("code")
} else if (file.exists("run_all.R")) {
  # Already in code/ directory
  cat("Already in code/ directory\n")
}

cat("Working directory:", getwd(), "\n\n")

# Safe source function with error handling
safe_source <- function(script_path, desc = "") {
  if (desc != "") cat("\n", desc, "\n")
  cat("Running:", script_path, "\n")
  
  tryCatch({
    source(script_path)
    cat("✓ Completed\n")
    return(TRUE)
  }, error = function(e) {
    cat("✗ Error:", e$message, "\n")
    return(FALSE)
  })
}

# Track successful and failed scripts
results <- list(success = c(), failed = c())

# Figure 1 - Pathogen contribution analysis
cat("----------------------------------------\n")
cat("Figure 1: Pathogen Contribution Analysis\n")
cat("----------------------------------------\n")
if (safe_source("Figure1/Figure1A.R")) results$success <- c(results$success, "Figure1A") else results$failed <- c(results$failed, "Figure1A")
if (safe_source("Figure1/Figure1B.R")) results$success <- c(results$success, "Figure1B") else results$failed <- c(results$failed, "Figure1B")
if (safe_source("Figure1/Figure1C.R")) results$success <- c(results$success, "Figure1C") else results$failed <- c(results$failed, "Figure1C")
if (safe_source("Figure1/Figure1D.R")) results$success <- c(results$success, "Figure1D") else results$failed <- c(results$failed, "Figure1D")

# Figure 2 - Geographic distribution
cat("\n----------------------------------------\n")
cat("Figure 2: Geographic Distribution\n")
cat("----------------------------------------\n")
if (safe_source("Figure2/Figure2A.R")) results$success <- c(results$success, "Figure2A") else results$failed <- c(results$failed, "Figure2A")
if (safe_source("Figure2/Figure2B.R")) results$success <- c(results$success, "Figure2B") else results$failed <- c(results$failed, "Figure2B")
if (safe_source("Figure2/Figure2C.R")) results$success <- c(results$success, "Figure2C") else results$failed <- c(results$failed, "Figure2C")
if (safe_source("Figure2/Figure2D.R")) results$success <- c(results$success, "Figure2D") else results$failed <- c(results$failed, "Figure2D")

# Figure 3 - Age-specific trends
cat("\n----------------------------------------\n")
cat("Figure 3: Age-Specific Trends\n")
cat("----------------------------------------\n")
if (safe_source("Figure3/Figure3A.R")) results$success <- c(results$success, "Figure3A") else results$failed <- c(results$failed, "Figure3A")
if (safe_source("Figure3/Figure3B.R")) results$success <- c(results$success, "Figure3B") else results$failed <- c(results$failed, "Figure3B")
if (safe_source("Figure3/Figure3C.R")) results$success <- c(results$success, "Figure3C") else results$failed <- c(results$failed, "Figure3C")

# Figure 4 - APC decomposition
cat("\n----------------------------------------\n")
cat("Figure 4: APC Effect Decomposition\n")
cat("----------------------------------------\n")
if (safe_source("Figure4/age effect.R")) results$success <- c(results$success, "Figure4_age") else results$failed <- c(results$failed, "Figure4_age")
if (safe_source("Figure4/period effect.R")) results$success <- c(results$success, "Figure4_period") else results$failed <- c(results$failed, "Figure4_period")
if (safe_source("Figure4/Cohort effect.R")) results$success <- c(results$success, "Figure4_cohort") else results$failed <- c(results$failed, "Figure4_cohort")

# Figure 5 - Pathogen-specific analysis (Associated deaths)
cat("\n----------------------------------------\n")
cat("Figure 5: Pathogen-Specific Analysis (Associated)\n")
cat("----------------------------------------\n")
if (safe_source("Figure5/Fig_left.R")) results$success <- c(results$success, "Figure5_left") else results$failed <- c(results$failed, "Figure5_left")
if (safe_source("Figure5/Fig_middle1.R")) results$success <- c(results$success, "Figure5_middle1") else results$failed <- c(results$failed, "Figure5_middle1")
if (safe_source("Figure5/Fig_middle2.R")) results$success <- c(results$success, "Figure5_middle2") else results$failed <- c(results$failed, "Figure5_middle2")
if (safe_source("Figure5/Fig_middle3.R")) results$success <- c(results$success, "Figure5_middle3") else results$failed <- c(results$failed, "Figure5_middle3")
if (safe_source("Figure5/Fig_right1.R")) results$success <- c(results$success, "Figure5_right1") else results$failed <- c(results$failed, "Figure5_right1")

# Figure S1 - Pathogen-specific analysis (Attributable deaths)
cat("\n----------------------------------------\n")
cat("Figure S1: Pathogen-Specific Analysis (Attributable)\n")
cat("----------------------------------------\n")
if (safe_source("FigureS1/Fig_left.R")) results$success <- c(results$success, "FigureS1_left") else results$failed <- c(results$failed, "FigureS1_left")
if (safe_source("FigureS1/Fig_middle1.R")) results$success <- c(results$success, "FigureS1_middle1") else results$failed <- c(results$failed, "FigureS1_middle1")
if (safe_source("FigureS1/Fig_middle2.R")) results$success <- c(results$success, "FigureS1_middle2") else results$failed <- c(results$failed, "FigureS1_middle2")
if (safe_source("FigureS1/Fig_middle3.R")) results$success <- c(results$success, "FigureS1_middle3") else results$failed <- c(results$failed, "FigureS1_middle3")
if (safe_source("FigureS1/Fig_right1.R")) results$success <- c(results$success, "FigureS1_right1") else results$failed <- c(results$failed, "FigureS1_right1")

# Figure 6 - COVID-19 impact analysis
cat("\n----------------------------------------\n")
cat("Figure 6: COVID-19 Impact Analysis\n")
cat("Note: 16 scripts require 5-8 hours\n")
cat("----------------------------------------\n")

# Option 1: Run all 16 scripts (uncomment to enable)
# figure6_files <- list.files("Figure6", pattern = "^Figure6_.*_.*\\.R$", full.names = TRUE)
# for (script in figure6_files) {
#   if (safe_source(script)) {
#     results$success <- c(results$success, basename(script))
#   } else {
#     results$failed <- c(results$failed, basename(script))
#   }
# }

# Option 2: Run representative pathogen only (fastest)
cat("Running representative analysis (1 script)...\n")
if (safe_source("Figure6/Figure6_Acinetobacter_baumannii_Associated.R")) results$success <- c(results$success, "Figure6_Abau_Ass") else results$failed <- c(results$failed, "Figure6_Abau_Ass")

# Display summary
cat("\n========================================\n")
cat("Execution Summary\n")
cat("========================================\n")
cat("Successful:", length(results$success), "scripts\n")
cat("Failed:", length(results$failed), "scripts\n\n")

if (length(results$failed) > 0) {
  cat("Failed scripts:\n")
  for (f in results$failed) {
    cat(" -", f, "\n")
  }
  cat("\n")
}

cat("Output files saved to /results directory\n")
cat("========================================\n")
cat("Analysis completed at:", as.character(Sys.time()), "\n")
cat("========================================\n")
