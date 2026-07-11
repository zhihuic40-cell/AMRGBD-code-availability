library(tidyverse)
library(easyGBDR)
GBD_edition(2021)

draw_utils_path <- c("code/Sensitivity/TableS8/draw_utils.R", "draw_utils.R")
draw_utils_path <- draw_utils_path[file.exists(draw_utils_path)][1]
if (is.na(draw_utils_path) || !nzchar(draw_utils_path)) {
  stop("Could not locate draw_utils.R")
}
source(draw_utils_path)

paths <- get_draw_pipeline_paths()
n_draws <- as.integer(Sys.getenv("APC_DRAW_COUNT", "500"))

types <- c("ass", "att")
all_results <- list()
all_failures <- list()

for (type in types) {
  draw_file <- file.path(paths$draw_data_dir, "table2", sprintf("table2_%s_draws.rds", type))
  if (!file.exists(draw_file)) {
    cat(sprintf("[SKIP] %s not found\n", basename(draw_file)))
    next
  }

  cat(sprintf("\n========== %s ==========\n", type))
  draw_df_all <- as.data.frame(readRDS(draw_file), stringsAsFactors = FALSE)
  draw_split <- split(draw_df_all, draw_df_all$draw_id, drop = TRUE)
  cause_name <- unique(draw_df_all$cause)[1]
  rei_name <- unique(draw_df_all$rei)[1]

  target_results <- list()
  target_failures <- list()

  for (draw_id in seq_len(n_draws)) {
    if (draw_id %% 25 == 0 || draw_id == 1 || draw_id == n_draws) {
      cat(sprintf("  Draw %d / %d ...\n", draw_id, n_draws))
    }

    draw_key <- as.character(draw_id)
    if (!draw_key %in% names(draw_split)) {
      target_failures[[length(target_failures) + 1]] <- data.frame(
        type = type,
        draw_id = draw_id,
        error_message = "Draw dataset not found.",
        stringsAsFactors = FALSE
      )
      next
    }

    draw_df <- draw_split[[draw_key]][, setdiff(names(draw_split[[draw_key]]), "draw_id"), drop = FALSE]
    draw_df <- draw_df[draw_df$val != 0, , drop = FALSE]
    location_names <- unique(draw_df$location)

    result <- tryCatch({
      apc_result <- GBDapc_web(
        data = draw_df,
        startyear = 1990,
        endyear = 2019,
        reference_age = 15,
        reference_year = 1990,
        measure_name = "Deaths",
        cause_name = cause_name,
        sex_name = "Both",
        location_name = location_names,
        rei_name = rei_name
      )

      nd <- apc_result$NetDrift
      nd$type <- type
      nd$draw_id <- draw_id
      nd
    }, error = function(e) {
      target_failures[[length(target_failures) + 1]] <<- data.frame(
        type = type,
        draw_id = draw_id,
        error_message = conditionMessage(e),
        stringsAsFactors = FALSE
      )
      NULL
    })

    if (!is.null(result)) {
      target_results[[length(target_results) + 1]] <- result
    }
  }

  if (length(target_results) > 0) {
    target_draws <- bind_rows(target_results)
    target_csv <- file.path(paths$draw_results_dir, sprintf("table2_%s_netdrift_draws.csv", type))
    write.csv(target_draws, target_csv, row.names = FALSE)
    all_results[[length(all_results) + 1]] <- target_draws
    cat(sprintf("  -> Saved draw-level net drift: %s (%d rows)\n", basename(target_csv), nrow(target_draws)))
  }

  if (length(target_failures) > 0) {
    target_fail_df <- bind_rows(target_failures)
    fail_csv <- file.path(paths$draw_results_dir, sprintf("table2_%s_failures.csv", type))
    write.csv(target_fail_df, fail_csv, row.names = FALSE)
    all_failures[[length(all_failures) + 1]] <- target_fail_df
    cat(sprintf("  -> Saved failures: %s (%d rows)\n", basename(fail_csv), nrow(target_fail_df)))
  }
}

if (length(all_results) > 0) {
  raw_draws <- bind_rows(all_results)
  raw_csv <- file.path(paths$draw_results_dir, "table2_netdrift_draws_all.csv")
  write.csv(raw_draws, raw_csv, row.names = FALSE)

  summary_df <- summarize_net_drift_draws(
    draw_results = raw_draws,
    group_cols = c("type", "location", "cause", "measure", "sex", "rei")
  )
  summary_csv <- file.path(paths$results_dir, "table2_draw500_netdrift_summary.csv")
  write.csv(summary_df, summary_csv, row.names = FALSE)

  cat(sprintf("\n✓ Table2 draw-level summary saved: %s (%d rows)\n", summary_csv, nrow(summary_df)))
} else {
  cat("\n[WARNING] No Table2 draw-level results generated.\n")
}

if (length(all_failures) > 0) {
  failure_df <- bind_rows(all_failures)
  failure_csv <- file.path(paths$results_dir, "table2_draw500_failures.csv")
  write.csv(failure_df, failure_csv, row.names = FALSE)
  cat(sprintf("✓ Table2 draw-level failures saved: %s (%d rows)\n", failure_csv, nrow(failure_df)))
}
