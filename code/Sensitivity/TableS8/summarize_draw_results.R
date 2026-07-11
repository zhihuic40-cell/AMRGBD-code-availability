library(tidyverse)

draw_utils_path <- c("code/Sensitivity/TableS8/draw_utils.R", "draw_utils.R")
draw_utils_path <- draw_utils_path[file.exists(draw_utils_path)][1]
if (is.na(draw_utils_path) || !nzchar(draw_utils_path)) {
  stop("Could not locate draw_utils.R")
}
source(draw_utils_path)

paths <- get_draw_pipeline_paths()

table1_file <- file.path(paths$results_dir, "table1_draw500_netdrift_summary.csv")
table2_file <- file.path(paths$results_dir, "table2_draw500_netdrift_summary.csv")

if (file.exists(table1_file)) {
  t1 <- read.csv(table1_file, stringsAsFactors = FALSE)
  t1_out <- t1 %>%
    select(pathogen, type, draws_completed, mc_95ui, apc_ci_median) %>%
    arrange(type, pathogen) %>%
    rename(Pathogen = pathogen)

  out_file <- file.path(paths$results_dir, "table1_draw500_comparison.csv")
  write.csv(t1_out, out_file, row.names = FALSE)
  cat(sprintf("✓ Table1 draw-level comparison: %s\n", out_file))
}

if (file.exists(table2_file)) {
  t2 <- read.csv(table2_file, stringsAsFactors = FALSE)
  t2_out <- t2 %>%
    select(location, type, draws_completed, mc_95ui, apc_ci_median) %>%
    arrange(type, location) %>%
    rename(region = location)

  out_file <- file.path(paths$results_dir, "table2_draw500_comparison.csv")
  write.csv(t2_out, out_file, row.names = FALSE)
  cat(sprintf("✓ Table2 draw-level comparison: %s\n", out_file))
}

cat("✓ Draw-level summary complete.\n")
