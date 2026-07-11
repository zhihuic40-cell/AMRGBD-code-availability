cat("====================================================\n")
cat("  Starting draw-level APC uncertainty propagation\n")
cat("====================================================\n\n")

if (file.exists("code/Sensitivity/TableS8/prepare_draw_inputs.R")) {
  script_dir <- "code/Sensitivity/TableS8"
} else if (file.exists("prepare_draw_inputs.R")) {
  script_dir <- "."
} else {
  stop("Please run this script from the repository root or code/Sensitivity/TableS8/ directory.")
}

cat("--- Part 1/4: Prepare 500-draw APC inputs ---\n")
source(file.path(script_dir, "prepare_draw_inputs.R"))

cat("\n--- Part 2/4: Table 2 draw-level APC ---\n")
source(file.path(script_dir, "run_table2_draws.R"))

cat("\n--- Part 3/4: Table 1 draw-level APC ---\n")
source(file.path(script_dir, "run_table1_draws.R"))

cat("\n--- Part 4/4: Summarize draw-level outputs ---\n")
source(file.path(script_dir, "summarize_draw_results.R"))

cat("\n====================================================\n")
cat("  Draw-level APC uncertainty propagation completed!\n")
cat("====================================================\n")
