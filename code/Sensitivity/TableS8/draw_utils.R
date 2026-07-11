library(tidyverse)

get_draw_pipeline_paths <- function() {
  is_code_ocean <- dir.exists("/code") && dir.exists("/data") && dir.exists("/results")

  if (is_code_ocean) {
    data_root <- "/data/sensitivity_easyGBDR"
    results_dir <- "/results/Sensitivity/TableS8"
  } else if (dir.exists("data/sensitivity_easyGBDR")) {
    data_root <- "data/sensitivity_easyGBDR"
    results_dir <- "results/Sensitivity/TableS8"
  } else if (dir.exists("../../data/sensitivity_easyGBDR")) {
    data_root <- "../../data/sensitivity_easyGBDR"
    results_dir <- "../../results/Sensitivity/TableS8"
  } else {
    stop("Cannot locate data/sensitivity_easyGBDR directory.")
  }

  draw_data_dir <- file.path(data_root, "draws")
  draw_results_dir <- file.path(results_dir, "draws")

  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(draw_data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(draw_results_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(draw_data_dir, "table1"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(draw_data_dir, "table2"), recursive = TRUE, showWarnings = FALSE)

  list(
    data_root = data_root,
    table1_dir = file.path(data_root, "table1"),
    table2_dir = file.path(data_root, "table2"),
    draw_data_dir = draw_data_dir,
    draw_results_dir = draw_results_dir,
    results_dir = results_dir
  )
}

read_sensitivity_csv <- function(filepath) {
  read.csv(filepath, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE) %>%
    rename(
      measure = measure_name,
      location = location_name,
      sex = sex_name,
      age = age_name,
      cause = cause_name,
      rei = rei_name,
      metric = metric_name
    ) %>%
    select(measure, location, sex, age, cause, rei, metric, year, val, upper, lower)
}

filter_apc_input <- function(data) {
  data %>%
    mutate(
      year = as.integer(.data$year),
      val = as.numeric(.data$val),
      upper = as.numeric(.data$upper),
      lower = as.numeric(.data$lower)
    ) %>%
    filter(.data$metric == "Number") %>%
    filter(!.data$age %in% c("All ages", "Age-standardized")) %>%
    filter(is.finite(.data$val), is.finite(.data$upper), is.finite(.data$lower))
}

infer_lognorm_params <- function(q_lower, q_upper, p_lower = 0.025, p_upper = 0.975) {
  eps <- 1e-8
  q_lower <- pmax(q_lower, eps)
  q_upper <- pmax(q_upper, q_lower + eps)
  z_l <- qnorm(p_lower)
  z_u <- qnorm(p_upper)
  sdlog <- (log(q_upper) - log(q_lower)) / (z_u - z_l)
  meanlog <- log(q_lower) - z_l * sdlog
  list(meanlog = meanlog, sdlog = sdlog)
}

generate_apc_draws <- function(input_df,
                               n_draws = 500L,
                               seed = 20260411L,
                               global_weight = 0,
                               location_weight = 0,
                               year_weight = 0,
                               age_weight = 0) {
  if (n_draws < 1) {
    stop("n_draws must be at least 1.")
  }

  weights <- c(global_weight, location_weight, year_weight, age_weight)
  if (any(weights < 0) || sum(weights) >= 1) {
    stop("Weights must be non-negative and sum to less than 1.")
  }

  base_df <- input_df %>%
    mutate(
      val = pmax(as.numeric(.data$val), 1e-8),
      lower = pmax(as.numeric(.data$lower), 1e-8),
      upper = pmax(as.numeric(.data$upper), .data$lower + 1e-8),
      location = as.character(.data$location),
      age = as.character(.data$age),
      year = as.integer(.data$year)
    )

  valid_mask <- with(base_df, is.finite(val) & is.finite(lower) & is.finite(upper) & upper > lower)
  params <- infer_lognorm_params(base_df$lower[valid_mask], base_df$upper[valid_mask])
  target_meanlog <- rep(NA_real_, nrow(base_df))
  target_meanlog[valid_mask] <- log(base_df$val[valid_mask]) - 0.5 * params$sdlog^2
  sdlog <- rep(NA_real_, nrow(base_df))
  sdlog[valid_mask] <- params$sdlog

  residual_weight <- max(1 - sum(weights), 1e-8)

  set.seed(seed)
  draw_list <- vector("list", n_draws)

  for (draw_id in seq_len(n_draws)) {
    row_shocks <- stats::rnorm(nrow(base_df))

    # Keep Figure 6 and APC sensitivity aligned: this github414 copy uses
    # a no-shared perturbation design, so all uncertainty is row-level noise.
    joint_z <- sqrt(residual_weight) * row_shocks

    draw_val <- base_df$val
    draw_val[valid_mask] <- exp(target_meanlog[valid_mask] + sdlog[valid_mask] * joint_z[valid_mask])

    draw_df <- base_df
    draw_df$draw_id <- draw_id
    draw_df$val <- round(pmax(draw_val, 0))
    draw_list[[draw_id]] <- draw_df
  }

  bind_rows(draw_list) %>%
    select("draw_id", everything())
}

extract_net_drift_cols <- function(df) {
  point_col <- grep("^Net Drift|^Net.Drift", names(df), value = TRUE)[1]
  lo_col <- grep("CILo|CI.Low", names(df), value = TRUE)[1]
  hi_col <- grep("CIHi|CI.High", names(df), value = TRUE)[1]

  if (is.na(point_col) || is.na(lo_col) || is.na(hi_col)) {
    stop("Could not identify Net Drift columns in APC output.")
  }

  list(point = point_col, lo = lo_col, hi = hi_col)
}

summarize_net_drift_draws <- function(draw_results, group_cols) {
  cols <- extract_net_drift_cols(draw_results)

  draw_results %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      draws_completed = dplyr::n(),
      net_drift_median = stats::median(.data[[cols$point]], na.rm = TRUE),
      net_drift_lower = as.numeric(stats::quantile(.data[[cols$point]], 0.025, na.rm = TRUE)),
      net_drift_upper = as.numeric(stats::quantile(.data[[cols$point]], 0.975, na.rm = TRUE)),
      apc_cilo_median = stats::median(.data[[cols$lo]], na.rm = TRUE),
      apc_cihi_median = stats::median(.data[[cols$hi]], na.rm = TRUE),
      net_drift_sd = stats::sd(.data[[cols$point]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      mc_95ui = sprintf("%.2f (%.2f, %.2f)", .data$net_drift_median, .data$net_drift_lower, .data$net_drift_upper),
      apc_ci_median = sprintf("%.2f, %.2f", .data$apc_cilo_median, .data$apc_cihi_median)
    )
}
