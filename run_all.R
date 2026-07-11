# ============================================================
# Master script: reproduce all figures from the repository root
# ============================================================
# Usage:
#   1. Open AMRGBD.Rproj in RStudio  (sets working directory automatically)
#   2. source("run_all.R")
#
#   Or from the command line:
#     cd AMRGBD-code-availability
#     Rscript run_all.R
# ============================================================

cat("========================================\n")
cat("AMRGBD — Reproduce All Figures\n")
cat("========================================\n\n")
cat("Working directory:", getwd(), "\n\n")

if (!dir.exists("data") || !dir.exists("code")) {
  stop("Please run this script from the repository root directory ",
       "(the folder containing 'data/' and 'code/').\n",
       "Tip: open AMRGBD.Rproj in RStudio, then source(\"run_all.R\").")
}

source("code/run_all.R")
