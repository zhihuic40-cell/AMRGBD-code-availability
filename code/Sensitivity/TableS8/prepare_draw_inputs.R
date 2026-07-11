library(tidyverse)

draw_utils_path <- c("code/Sensitivity/TableS8/draw_utils.R", "draw_utils.R")
draw_utils_path <- draw_utils_path[file.exists(draw_utils_path)][1]
if (is.na(draw_utils_path) || !nzchar(draw_utils_path)) {
  stop("Could not locate draw_utils.R")
}
source(draw_utils_path)

paths <- get_draw_pipeline_paths()
n_draws <- as.integer(Sys.getenv("APC_DRAW_COUNT", "500"))
base_seed <- as.integer(Sys.getenv("APC_DRAW_SEED", "20260411"))

PATHOGENS <- list(
  list(full = "Acinetobacter baumannii", short = "Acinetobacter_baumannii"),
  list(full = "Citrobacter spp.", short = "Citrobacter_spp"),
  list(full = "Enterobacter spp.", short = "Enterobacter_spp"),
  list(full = "Escherichia coli", short = "Escherichia_coli"),
  list(full = "Klebsiella pneumoniae", short = "Klebsiella_pneumoniae"),
  list(full = "Pseudomonas aeruginosa", short = "Pseudomonas_aeruginosa"),
  list(full = "Serratia spp.", short = "Serratia_spp")
)

types <- c("ass", "att")

cat("====================================================\n")
cat("Preparing draw-level easyGBDR APC inputs\n")
cat("====================================================\n")
cat("Draw count :", n_draws, "\n")
cat("Base seed  :", base_seed, "\n\n")

for (type_idx in seq_along(types)) {
  type <- types[[type_idx]]
  table2_file <- file.path(paths$table2_dir, sprintf("table2_%s_val.csv", type))
  table2_out <- file.path(paths$draw_data_dir, "table2", sprintf("table2_%s_draws.rds", type))
  table2_df <- read_sensitivity_csv(table2_file) %>% filter_apc_input()
  table2_draws <- generate_apc_draws(
    input_df = table2_df,
    n_draws = n_draws,
    seed = base_seed + type_idx
  )
  saveRDS(table2_draws, table2_out)
  cat(sprintf("✓ Table2 %-3s draws saved: %s (%d rows)\n", type, basename(table2_out), nrow(table2_draws)))

  for (pathogen_idx in seq_along(PATHOGENS)) {
    pathogen <- PATHOGENS[[pathogen_idx]]
    table1_file <- file.path(paths$table1_dir, sprintf("table1_%s_%s_val.csv", type, pathogen$short))
    table1_out <- file.path(paths$draw_data_dir, "table1", sprintf("table1_%s_%s_draws.rds", type, pathogen$short))
    table1_df <- read_sensitivity_csv(table1_file) %>% filter_apc_input()
    table1_draws <- generate_apc_draws(
      input_df = table1_df,
      n_draws = n_draws,
      seed = base_seed + type_idx * 100 + pathogen_idx
    )
    saveRDS(table1_draws, table1_out)
    cat(sprintf("✓ Table1 %-3s %-28s draws saved: %s (%d rows)\n",
                type, pathogen$short, basename(table1_out), nrow(table1_draws)))
  }
}

cat("\nAll draw inputs prepared under:\n")
cat(paths$draw_data_dir, "\n")
