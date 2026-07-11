VERSION_SUFFIX <- "_input_uncertainty_noshared_fixedparams"
ORIGINAL_SCRIPT_NAME <- "Figure6_Enterobacter_spp_Associated.R"
INPUT_DRAW_COUNT <- 500L

get_current_script_stem <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(sub("\\.R$", "", basename(sub("^--file=", "", file_arg[1]))))
  }

  paste0(sub("\\.R$", "", ORIGINAL_SCRIPT_NAME), "_input_uncertainty")
}

DIAGNOSTIC_LOG_FILE <- file.path(
  "logs_input_uncertainty",
  paste0("diag_", get_current_script_stem(), ".log")
)

diag_log <- function(...) {
  log_dir <- dirname(DIAGNOSTIC_LOG_FILE)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }

  msg <- paste0(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
    paste(..., collapse = "")
  )
  cat(msg, "\n", file = DIAGNOSTIC_LOG_FILE, append = TRUE)
}

infer_lognorm_params <- function(q_lower, q_upper, p_lower = 0.025, p_upper = 0.975) {
  eps <- 1e-12
  q_lower <- max(q_lower, eps)
  q_upper <- max(q_upper, q_lower + eps)
  z_l <- qnorm(p_lower)
  z_u <- qnorm(p_upper)
  sdlog <- (log(q_upper) - log(q_lower)) / (z_u - z_l)
  meanlog <- (log(q_upper) + log(q_lower)) / 2
  list(meanlog = meanlog, sdlog = sdlog)
}

normalize_prediction_years <- function(raw_years, expected_years) {
  raw_years <- as.numeric(raw_years)
  expected_years <- as.numeric(expected_years)

  if (length(raw_years) != length(expected_years)) {
    stop("Raw prediction years and expected years must have the same length.")
  }

  if (all(raw_years == expected_years)) {
    return(raw_years)
  }

  overlap_n <- sum(raw_years %in% expected_years)
  if (overlap_n < length(expected_years) / 2) {
    return(expected_years)
  }

  raw_years
}

resolve_original_script <- function() {
  candidates <- c(
    ORIGINAL_SCRIPT_NAME,
    file.path("Figure6", ORIGINAL_SCRIPT_NAME),
    file.path("code", "Figure6", ORIGINAL_SCRIPT_NAME)
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop(sprintf("Could not locate the original Figure 6 script: %s", ORIGINAL_SCRIPT_NAME))
  }
  existing[1]
}

resolve_fixed_param_file <- function(pathogen_name, counterfactual_type) {
  output_base <- paste0(gsub(" ", "_", pathogen_name), "_", counterfactual_type, "_input_uncertainty_noshared")
  filename <- paste0(output_base, "_optimal_parameters.csv")

  candidates <- c(
    file.path("results", "Figure6", filename),
    file.path("..", "results", "Figure6", filename),
    file.path("..", "..", "results", "Figure6", filename),
    file.path("E:/github1/results/Figure6", filename)
  )

  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop(sprintf("Could not locate fixed parameter file: %s", filename))
  }

  existing[1]
}

load_fixed_optimal_params <- function(pathogen_name, counterfactual_type) {
  param_file <- resolve_fixed_param_file(pathogen_name, counterfactual_type)
  param_table <- utils::read.csv(param_file, stringsAsFactors = FALSE)

  if (nrow(param_table) == 0) {
    stop(sprintf("Fixed parameter file is empty: %s", param_file))
  }

  for (nm in c("gf", "rmse", "mae", "n_obs", "param_combination")) {
    if (nm %in% names(param_table)) {
      param_table[[nm]] <- suppressWarnings(as.numeric(param_table[[nm]]))
    }
  }
  if ("secondDiff" %in% names(param_table)) {
    param_table$secondDiff <- as.logical(param_table$secondDiff)
  }

  list(
    param_file = param_file,
    optimal_params = param_table[1, , drop = FALSE],
    all_results = param_table
  )
}

sample_history_draw <- function(history_df,
                                year_shared_weight = 0,
                                age_shared_weight = 0,
                                seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  history_df$year <- as.numeric(history_df$year)
  year_levels <- sort(unique(history_df$year))
  age_levels <- unique(history_df$age)

  year_shared_weight <- min(max(year_shared_weight, 0), 0.95)
  age_shared_weight <- min(max(age_shared_weight, 0), 0.95 - year_shared_weight)
  residual_weight <- max(1 - year_shared_weight - age_shared_weight, 1e-6)

  year_shocks <- stats::rnorm(length(year_levels))
  names(year_shocks) <- as.character(year_levels)
  age_shocks <- stats::rnorm(length(age_levels))
  names(age_shocks) <- age_levels
  row_noise <- stats::rnorm(nrow(history_df))

  history_df$draw_val <- mapply(
    FUN = function(val, lower, upper, year, age, noise) {
      eps <- 1e-12
      val <- max(as.numeric(val), eps)
      lower <- as.numeric(lower)
      upper <- as.numeric(upper)

      if (!is.finite(lower) || !is.finite(upper) || lower <= 0 || upper <= lower) {
        return(val)
      }

      params <- infer_lognorm_params(lower, upper)
      target_meanlog <- log(val) - 0.5 * params$sdlog^2
      joint_z <- sqrt(year_shared_weight) * year_shocks[as.character(year)] +
        sqrt(age_shared_weight) * age_shocks[age] +
        sqrt(residual_weight) * noise

      exp(target_meanlog + params$sdlog * joint_z)
    },
    val = history_df$val,
    lower = history_df$lower,
    upper = history_df$upper,
    year = history_df$year,
    age = history_df$age,
    noise = row_noise
  )

  history_df
}

build_draw_matrix <- function(draw_df, ages_vector, year_vector) {
  mat <- reshape2::dcast(draw_df, year ~ age, value.var = "draw_val")
  rownames(mat) <- mat$year
  mat <- mat[, setdiff(names(mat), "year"), drop = FALSE]

  missing_cols <- setdiff(ages_vector, names(mat))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      mat[[col]] <- NA_real_
    }
  }

  mat <- mat[, ages_vector, drop = FALSE]
  mat <- as.matrix(mat)
  rownames(mat) <- year_vector
  mat <- round(pmax(mat, 0))
  as.data.frame(mat)
}

extract_prediction_means <- function(bapc_result, expected_years, target_years) {
  if ("ASR" %in% names(bapc_result)) {
    predicted_asr <- as.data.frame(bapc_result$ASR) * 10^5
  } else {
    predicted_asr <- as.data.frame(BAPC::agestd.rate(bapc_result)) * 10^5
  }

  predicted_asr$year <- normalize_prediction_years(
    raw_years = rownames(predicted_asr),
    expected_years = expected_years
  )
  predicted_asr <- predicted_asr[predicted_asr$year %in% target_years, c("year", "mean"), drop = FALSE]
  predicted_asr <- predicted_asr[order(predicted_asr$year), , drop = FALSE]

  if (!all(predicted_asr$year == target_years)) {
    stop("Could not extract the requested prediction years from the BAPC output.")
  }

  as.numeric(predicted_asr$mean)
}

summarize_input_uncertainty_results <- function(predicted_draw_matrix,
                                                prediction_years,
                                                observed_values) {
  predicted_draw_matrix <- as.matrix(predicted_draw_matrix)
  prediction_years <- as.numeric(prediction_years)
  observed_values <- as.numeric(observed_values)

  point_predicted <- apply(predicted_draw_matrix, 2, stats::median, na.rm = TRUE)
  rows <- vector("list", length(prediction_years))

  for (i in seq_along(prediction_years)) {
    ard_draws <- observed_values[i] - predicted_draw_matrix[, i]
    rrd_draws <- ard_draws / predicted_draw_matrix[, i] * 100
    point_ard <- observed_values[i] - point_predicted[i]
    point_rrd <- point_ard / point_predicted[i] * 100

    rows[[i]] <- data.frame(
      Periods = prediction_years[i],
      Observed = sprintf("%.2f", observed_values[i]),
      Predicted = sprintf("%.2f", point_predicted[i]),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    rows[[i]]$`Absolute difference (95%CI)` <- sprintf(
      "%.2f (%.2f, %.2f)",
      point_ard,
      as.numeric(quantile(ard_draws, 0.025, na.rm = TRUE)),
      as.numeric(quantile(ard_draws, 0.975, na.rm = TRUE))
    )
    rows[[i]]$`Relative change (95%CI)` <- sprintf(
      "%.2f%% (%.2f%%, %.2f%%)",
      point_rrd,
      as.numeric(quantile(rrd_draws, 0.025, na.rm = TRUE)),
      as.numeric(quantile(rrd_draws, 0.975, na.rm = TRUE))
    )
  }

  combined_predicted_draws <- rowMeans(predicted_draw_matrix, na.rm = TRUE)
  observed_avg <- mean(observed_values, na.rm = TRUE)
  predicted_avg <- mean(point_predicted, na.rm = TRUE)
  combined_ard_draws <- observed_avg - combined_predicted_draws
  combined_rrd_draws <- combined_ard_draws / combined_predicted_draws * 100

  summary_row <- data.frame(
    Periods = "2020-2021",
    Observed = sprintf("%.2f", observed_avg),
    Predicted = sprintf("%.2f", predicted_avg),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  summary_row$`Absolute difference (95%CI)` <- sprintf(
    "%.2f (%.2f, %.2f)",
    observed_avg - predicted_avg,
    as.numeric(quantile(combined_ard_draws, 0.025, na.rm = TRUE)),
    as.numeric(quantile(combined_ard_draws, 0.975, na.rm = TRUE))
  )
  summary_row$`Relative change (95%CI)` <- sprintf(
    "%.2f%% (%.2f%%, %.2f%%)",
    (observed_avg - predicted_avg) / predicted_avg * 100,
    as.numeric(quantile(combined_rrd_draws, 0.025, na.rm = TRUE)),
    as.numeric(quantile(combined_rrd_draws, 0.975, na.rm = TRUE))
  )

  do.call(rbind, c(rows, list(summary_row)))
}

run_input_uncertainty_analysis <- function() {
  if (file.exists(DIAGNOSTIC_LOG_FILE)) {
    file.remove(DIAGNOSTIC_LOG_FILE)
  }
  diag_log("START run_input_uncertainty_analysis()")

  original_script_path <- resolve_original_script()
  diag_log("Resolved original script: ", original_script_path)
  original_lines <- readLines(original_script_path, warn = FALSE)
  section4_idx <- grep("Execute Optimization Workflow", original_lines)[1]
  section5_idx <- grep("Final Prediction Using Optimal Parameters", original_lines)[1]
  section7_idx <- grep("Calculate ASR Differences and Generate Output Table", original_lines)[1]
  if (!length(section4_idx) || is.na(section4_idx) ||
      !length(section5_idx) || is.na(section5_idx) ||
      !length(section7_idx) || is.na(section7_idx)) {
    stop("Could not identify the required split points in the original script.")
  }
  diag_log(
    "Section markers found: section4=", section4_idx,
    ", section5=", section5_idx,
    ", section7=", section7_idx
  )

  analysis_env <- new.env(parent = globalenv())
  diag_log("Evaluating original script pre-optimization section")
  eval(parse(text = original_lines[1:(section4_idx - 1)]), envir = analysis_env)
  diag_log("Finished evaluating pre-optimization section")

  EC2 <- get("EC2", envir = analysis_env)
  PATHOGEN_NAME <- get("PATHOGEN_NAME", envir = analysis_env)
  COUNTERFACTUAL_TYPE <- get("COUNTERFACTUAL_TYPE", envir = analysis_env)
  fixed_selection <- load_fixed_optimal_params(PATHOGEN_NAME, COUNTERFACTUAL_TYPE)
  diag_log(
    "Loaded fixed parameters from ", fixed_selection$param_file,
    "; gf=", fixed_selection$optimal_params$gf,
    "; secondDiff=", fixed_selection$optimal_params$secondDiff
  )
  assign("selection_result", fixed_selection, envir = analysis_env)
  assign("optimal_params", fixed_selection$optimal_params, envir = analysis_env)

  cat("\n========================================\n")
  cat("Using fixed optimal parameters for input-uncertainty run\n")
  cat("========================================\n")
  cat("Fixed parameter file:", fixed_selection$param_file, "\n")
  cat("gf:", fixed_selection$optimal_params$gf,
      "secondDiff:", fixed_selection$optimal_params$secondDiff, "\n\n")

  diag_log("Evaluating final-prediction section from original script")
  eval(parse(text = original_lines[section5_idx:(section7_idx - 1)]), envir = analysis_env)
  diag_log("Finished evaluating final-prediction section")

  standardize_age_labels <- get("standardize_age_labels", envir = analysis_env)
  ages <- get("ages", envir = analysis_env)
  ages_3 <- get("ages_3", envir = analysis_env)
  GBD_Global_Both_n <- get("GBD_Global_Both_n", envir = analysis_env)
  optimal_params <- get("optimal_params", envir = analysis_env)
  selection_result <- get("selection_result", envir = analysis_env)
  holdout_rmse <- get("holdout_rmse", envir = analysis_env)
  holdout_mae <- get("holdout_mae", envir = analysis_env)
  results_dir <- get("results_dir", envir = analysis_env)
  wstand <- get("wstand", envir = analysis_env)
  convert_to_bapc_config <- get("convert_to_bapc_config", envir = analysis_env)
  full_years <- as.numeric(get("full_years", envir = analysis_env))
  Male_ASR <- get("Male_ASR", envir = analysis_env)

  optimal_config <- convert_to_bapc_config(optimal_params)
  prediction_years <- c(2020, 2021)
  expected_years <- c(full_years, prediction_years)
  population_extended_final <- GBD_Global_Both_n[as.character(expected_years), ]
  diag_log(
    "Prepared model objects; expected_years=", paste(expected_years, collapse = ","),
    "; history years=", paste(range(full_years), collapse = "-")
  )

  history_input <- EC2[
    EC2$year <= 2019 &
      EC2$counterfactual == COUNTERFACTUAL_TYPE &
      EC2$measure == "Deaths" &
      EC2$pathogen == PATHOGEN_NAME,
  ]
  history_input <- standardize_age_labels(history_input)
  history_input <- history_input[
    history_input$age %in% ages &
      history_input$sex == "Both sexes" &
      history_input$metric == "Number" &
      history_input$measure == "Deaths" &
      history_input$location == "Global",
    c("year", "age", "val", "lower", "upper")
  ]

  actual_asr <- EC2[
    EC2$counterfactual == COUNTERFACTUAL_TYPE &
      EC2$measure == "Deaths" &
      EC2$pathogen == PATHOGEN_NAME &
      EC2$age == "Age-standardized" &
      EC2$metric == "Rate (per 100,000)" &
      EC2$year %in% prediction_years,
    c("year", "val")
  ]
  actual_asr <- actual_asr[order(actual_asr$year), , drop = FALSE]

  if (!all(actual_asr$year == prediction_years)) {
    stop("Observed 2020-2021 ASR values could not be aligned.")
  }
  diag_log(
    "Prepared input data; history_input rows=", nrow(history_input),
    "; actual_asr rows=", nrow(actual_asr)
  )

  predicted_draw_matrix <- matrix(
    NA_real_,
    nrow = INPUT_DRAW_COUNT,
    ncol = length(prediction_years),
    dimnames = list(NULL, as.character(prediction_years))
  )

  cat("\n========================================\n")
  cat("Running input-uncertainty propagation draws\n")
  cat("========================================\n")
  cat("Input draws:", INPUT_DRAW_COUNT, "\n")
  diag_log("Starting draw loop; INPUT_DRAW_COUNT=", INPUT_DRAW_COUNT)

  for (draw_idx in seq_len(INPUT_DRAW_COUNT)) {
    if (draw_idx %% 5 == 0 || draw_idx == 1 || draw_idx == INPUT_DRAW_COUNT) {
      cat("Input draw", draw_idx, "/", INPUT_DRAW_COUNT, "...\n")
    }

    draw_stage <- "start"
    draw_result <- tryCatch({
      diag_log("DRAW ", draw_idx, " START")

      draw_stage <- "sample_history_draw"
      draw_df <- sample_history_draw(
        history_df = history_input,
        year_shared_weight = 0,
        age_shared_weight = 0,
        seed = 1000 + draw_idx
      )
      diag_log("DRAW ", draw_idx, " sample_history_draw DONE")

      draw_stage <- "build_draw_matrix"
      draw_matrix <- build_draw_matrix(draw_df, ages_vector = ages_3, year_vector = full_years)
      diag_log("DRAW ", draw_idx, " build_draw_matrix DONE")

      draw_stage <- "extend_matrix"
      extended_death_matrix <- matrix(
        NA_real_,
        nrow = length(expected_years),
        ncol = ncol(draw_matrix),
        dimnames = list(as.character(expected_years), colnames(draw_matrix))
      )
      extended_death_matrix[as.character(full_years), ] <- as.matrix(draw_matrix)
      extended_death_matrix <- as.data.frame(extended_death_matrix)
      diag_log("DRAW ", draw_idx, " extend_matrix DONE")

      draw_stage <- "BAPC"
      diag_log("DRAW ", draw_idx, " BAPC START")
      draw_fit <- BAPC::BAPC(
        BAPC::APCList(extended_death_matrix, population_extended_final, gf = optimal_config$gf),
        predict = list(npredict = length(prediction_years), retro = FALSE),
        model = optimal_config$model,
        secondDiff = optimal_config$secondDiff,
        stdweight = wstand,
        verbose = FALSE
      )
      diag_log("DRAW ", draw_idx, " BAPC DONE")

      draw_stage <- "extract_prediction_means"
      draw_values <- extract_prediction_means(
        bapc_result = draw_fit,
        expected_years = expected_years,
        target_years = prediction_years
      )
      diag_log(
        "DRAW ", draw_idx, " extract_prediction_means DONE values=",
        paste(sprintf("%.4f", draw_values), collapse = ",")
      )
      draw_values
    }, error = function(e) {
      diag_log(
        "DRAW ", draw_idx, " ERROR stage=", draw_stage,
        " message=", conditionMessage(e)
      )
      cat(
        "Skipping input draw", draw_idx, "due to error:",
        conditionMessage(e), "\n"
      )
      rep(NA_real_, length(prediction_years))
    })

    predicted_draw_matrix[draw_idx, ] <- draw_result
    diag_log(
      "DRAW ", draw_idx, " END stored=",
      paste(ifelse(is.na(draw_result), "NA", sprintf("%.4f", draw_result)), collapse = ",")
    )
  }

  valid_rows <- stats::complete.cases(predicted_draw_matrix)
  diag_log("Draw loop complete; valid draws=", sum(valid_rows), "/", length(valid_rows))
  predicted_draw_matrix <- predicted_draw_matrix[valid_rows, , drop = FALSE]
  if (nrow(predicted_draw_matrix) < 10) {
    stop("Too few valid input-uncertainty draws were generated.")
  }

  asr_with_period <- summarize_input_uncertainty_results(
    predicted_draw_matrix = predicted_draw_matrix,
    prediction_years = prediction_years,
    observed_values = actual_asr$val
  )
  rmse_val <- suppressWarnings(as.numeric(holdout_rmse))
  mae_val <- suppressWarnings(as.numeric(holdout_mae))
  asr_with_period$RMSE <- sprintf("%.4f", rmse_val)
  asr_with_period$MAE <- sprintf("%.4f", mae_val)
  asr_with_period <- asr_with_period[, c(
    "RMSE",
    "MAE",
    "Periods",
    "Observed",
    "Predicted",
    "Absolute difference (95%CI)",
    "Relative change (95%CI)"
  )]

  output_base <- paste0(gsub(" ", "_", PATHOGEN_NAME), "_", COUNTERFACTUAL_TYPE, VERSION_SUFFIX)
  output_asr_file <- file.path(results_dir, paste0(output_base, "_asr_differences.csv"))
  write.csv(asr_with_period, output_asr_file, row.names = FALSE)
  diag_log("Saved ASR differences to ", output_asr_file)

  param_filename <- file.path(results_dir, paste0(output_base, "_optimal_parameters.csv"))
  if (nrow(selection_result$all_results) > 0) {
    write.csv(selection_result$all_results, param_filename, row.names = FALSE)
    cat("Parameter optimization results saved:", param_filename, "\n")
    diag_log("Saved fixed parameter table to ", param_filename)
  } else {
    cat("Used default parameters\n")
    diag_log("Selection results empty; no parameter table written")
  }

  Male_ASR$year <- normalize_prediction_years(rownames(Male_ASR), expected_years)
  EC_actual <- EC2[
    EC2$counterfactual == COUNTERFACTUAL_TYPE &
      EC2$measure == "Deaths" &
      EC2$pathogen == PATHOGEN_NAME &
      EC2$age == "Age-standardized" &
      EC2$metric == "Rate (per 100,000)",
    c("year", "val")
  ]
  names(EC_actual) <- c("year", "actual_rate")

  predicted_data <- data.frame(
    year = prediction_years,
    predicted_rate = apply(predicted_draw_matrix, 2, stats::median, na.rm = TRUE)
  )
  historical_pred <- Male_ASR[Male_ASR$year < min(prediction_years), c("year", "mean"), drop = FALSE]
  names(historical_pred) <- c("year", "predicted_rate")
  predicted_plot_data <- rbind(historical_pred, predicted_data)

  combined_asr <- merge(EC_actual, predicted_plot_data, by = "year", all = TRUE)
  combined_asr <- combined_asr[order(combined_asr$year), ]
  ep_data <- combined_asr[combined_asr$year >= 2010 & combined_asr$year <= 2021, ]
  max_value <- max(ep_data$actual_rate, ep_data$predicted_rate, na.rm = TRUE)
  upper_limit <- max_value * 1.1

  # nolint start: object_usage_linter
  p <- ggplot2::ggplot(ep_data, ggplot2::aes(x = .data$year)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.7),
      axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.7),
      axis.ticks.length = grid::unit(0.2, "cm"),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.x = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(t = 15, b = 5)),
      axis.title.y = ggplot2::element_text(size = 14, face = "bold", margin = ggplot2::margin(r = 15, l = 5)),
      axis.text.x = ggplot2::element_text(size = 12, face = "bold", color = "black"),
      axis.text.y = ggplot2::element_text(size = 12, face = "bold", color = "black"),
      legend.position = "none",
      plot.margin = ggplot2::margin(t = 25, r = 25, b = 25, l = 25, unit = "pt"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$actual_rate), color = "black", linewidth = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = .data$predicted_rate), color = "red", linewidth = 1.2, linetype = "solid") +
    ggplot2::geom_vline(xintercept = 2020, linetype = "dashed", linewidth = 0.8, color = "gray40") +
    ggplot2::geom_vline(xintercept = 2021, linetype = "dashed", linewidth = 0.8, color = "gray40") +
    ggplot2::labs(
      title = paste(PATHOGEN_NAME, "-", COUNTERFACTUAL_TYPE, "(input uncertainty)"),
      x = "Year",
      y = "Age-standardized mortality rate (per 100,000)"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(2010, 2021, 2), limits = c(2010, 2021.5)) +
    ggplot2::scale_y_continuous(limits = c(NA, upper_limit), expand = ggplot2::expansion(mult = c(0.05, 0.1)))
  # nolint end

  plot_filename <- paste0(output_base, "_prediction_plot.pdf")
  plot_file <- file.path(results_dir, plot_filename)
  ggplot2::ggsave(plot_file, p, width = 12, height = 8, units = "in", device = cairo_pdf, dpi = 300)
  cat(sprintf("✓ Plot saved to: %s\n", plot_file))
  diag_log("Saved plot to ", plot_file)

  cat("\n========================================\n")
  cat("Input-uncertainty analysis complete!\n")
  cat("Pathogen:", PATHOGEN_NAME, "\n")
  cat("Counterfactual:", COUNTERFACTUAL_TYPE, "\n")
  cat("Input draws:", nrow(predicted_draw_matrix), "\n")
  cat("Optimal parameters - gf:", optimal_params$gf, "secondDiff:", optimal_params$secondDiff, "\n")
  if (!is.na(optimal_params$rmse)) {
    cat("Validation performance - RMSE:", round(optimal_params$rmse, 4), "MAE:", round(optimal_params$mae, 4), "\n")
  } else {
    cat("Used default parameters\n")
  }
  cat("Results saved to:", output_asr_file, "\n")
  cat("Parameters saved to:", param_filename, "\n")
  cat("========================================\n")
  diag_log("END run_input_uncertainty_analysis()")
}

.common_candidates <- c(
  "TableS9_common.R",
  file.path("code", "Sensitivity", "TableS9", "TableS9_common.R"),
  file.path("Sensitivity", "TableS9", "TableS9_common.R"),
  file.path("TableS9", "TableS9_common.R")
)
.common_found <- .common_candidates[file.exists(.common_candidates)]
if (length(.common_found) > 0) {
  source(.common_found[1], local = TRUE)
} else {
  warning("TableS9_common.R not found; using inline function definitions")
}

if (!identical(Sys.getenv("FIGURE6_SKIP_MAIN"), "1")) {
  run_input_uncertainty_analysis()
}


