library(BAPC)
library(INLA)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(epitools)
library(reshape2)
library(here)

# Set pathogen for analysis
PATHOGEN_NAME <- 'Serratia spp.'

# =============================================================================
# 1. Data Preparation and Split Strategy
# =============================================================================

# Data split function
prepare_data_splits <- function(EC_data, train_end = 2016, validation_end = 2019) {
  return(list(
    training_period = 1990:train_end,
    validation_period = (train_end + 1):validation_end,
    prediction_period = (validation_end + 1):2021,
    train_end = train_end,
    validation_end = validation_end
  ))
}

# Set data splits
data_splits <- prepare_data_splits(EC_data = NULL, train_end = 2016, validation_end = 2019)
cat("Data split strategy:\n")
cat("Training period:", paste(data_splits$training_period[1], "-", max(data_splits$training_period)), "\n")
cat("Validation period:", paste(min(data_splits$validation_period), "-", max(data_splits$validation_period)), "\n")
cat("Prediction period:", paste(min(data_splits$prediction_period), "-", max(data_splits$prediction_period)), "\n\n")

EC <- read.csv(here("data.csv"))
EC2 <- EC

# Modify data filtering logic based on data split strategy
EC_full <- EC %>% dplyr::filter(counterfactual == "Associated" & measure == "Deaths"& pathogen== PATHOGEN_NAME)
EC_train <- EC_full %>% dplyr::filter(year <= data_splits$train_end)
EC_validation <- EC_full %>% dplyr::filter(year > data_splits$train_end & year <= data_splits$validation_end)

age_stand <- read.csv(here('GBD2021 world population age standard.csv'))[-c(1:3),]
rownames(age_stand) <- 1:nrow(age_stand)

# Age label standardization function
standardize_age_labels <- function(data) {
  data %>% 
    mutate(age = sub(' to ', replacement = '-', age)) %>% 
    mutate(age = paste0(age, ' years')) %>% 
    mutate(age = sub('95 plus', replacement = '95+', age)) %>% 
    mutate(age = sub('Under 5', replacement = '0-4', age)) %>% 
    dplyr::filter(val > 0)
}

# Apply age label standardization
EC_train <- standardize_age_labels(EC_train)
EC_validation <- standardize_age_labels(EC_validation)

# Define age structure
ages <- c("0-4 years","5-9 years","10-14 years","15-19 years","20-24 years",
            "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
            "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
            "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")

ages_2 <- c("<1 year","12-23 months","2-4 years","5-9 years","10-14 years","15-19 years","20-24 years",
            "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
            "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
            "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")

ages_3 <- c("0-4 years","5-9 years","10-14 years","15-19 years","20-24 years",
            "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
            "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
            "75-79 years","80-84 years","85-89 years","90-94 years","95+ years")

# Calculate standard weights
wstand <- c(age_stand$std_population[1:2] %>% as.numeric() %>% sum(),
            age_stand$std_population[3:21] %>% as.numeric())/sum(age_stand$std_population[1:21])

# =============================================================================
# 2. Population Data Processing
# =============================================================================

dirname <- dir(here("GBD_Population"))
file <- paste0(here("GBD_Population"), "/", dirname)
var_name <- c('location_id',"location_name","sex_name","year","age_name","val")

GBD_population  <-  as.data.frame(matrix(nrow=0,ncol=length(var_name)))
names(GBD_population)=var_name
for (a in file) {
  data <- fread(a) %>% as.data.frame() %>% dplyr::select(var_name) %>% 
    dplyr::filter(age_name %in% ages_2 & location_id !=533 & year <= 2019)
  GBD_population <- rbind(GBD_population,data)
}

GBD_population <- GBD_population %>% dplyr::select(-1)

# Process age group merging
GBD_2to4 <- GBD_population %>% 
  dplyr::filter(age_name %in% c("12-23 months", "2-4 years")) %>%
  group_by(location_name, sex_name, year) %>% 
  summarise(val = sum(val)) %>%
  mutate(age_name = "2-4 years") %>% 
  dplyr::select(var_name[-1])

GBD_population <- GBD_population %>% 
  dplyr::filter(!(age_name %in% c("12-23 months", "2-4 years")))
GBD_population <- rbind(GBD_population, GBD_2to4)

# Predict population data
prediction_var_name <- c("location_name","sex","year_id","age_group_name","val")
GBD_population_prediction <- fread(here('IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.CSV')) %>%
  dplyr::select(prediction_var_name) %>%
  dplyr::filter(year_id %in% 2020:2030)
names(GBD_population_prediction) <- var_name[-1]

# Process neonatal age groups
GBD_1year <- GBD_population_prediction %>% 
  dplyr::filter(age_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal")) %>%
  group_by(location_name,sex_name,year) %>% 
  summarise(val=sum(val)) %>%
  mutate(age_name="<1 year") %>% 
  dplyr::select(var_name[-1])

GBD_population_prediction <- GBD_population_prediction %>% 
  dplyr::filter(!(age_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal"))) %>%
  rbind(GBD_1year)

# Standardize age names
GBD_population_prediction$age_name <- gsub(" to ", "-", GBD_population_prediction$age_name)
GBD_population_prediction$age_name <- gsub("([0-9]+)-([0-9]+)$", "\\1-\\2 years", GBD_population_prediction$age_name)
GBD_population_prediction$age_name <- gsub("^([0-9]+) plus$", "\\1+ years", GBD_population_prediction$age_name)
GBD_population_prediction <- GBD_population_prediction[GBD_population_prediction$age_name != "All Ages", ]

# Merge population data
GBD <- rbind(GBD_population,GBD_population_prediction)

# Calculate 0-4 age group
GBD_age4 <- GBD %>% dplyr::filter(age_name %in% c("<1 year","2-4 years")) %>%
  group_by(location_name,sex_name,year) %>%
  summarize(val=sum(val)) %>%  mutate(age_name='0-4 years') %>%
  dplyr::select(var_name[-1])
GBD <- rbind(GBD,GBD_age4)

GBD <- subset(GBD, age_name %in% ages_3) %>%
  mutate(age_name=factor(age_name,levels=ages_3,ordered = T)) %>%
  arrange(age_name)

# Extract global population data
GBD_Global_Male <- GBD %>% dplyr::filter(location_name=='Global' & sex_name == 'Male')
GBD_Global_Female <-  GBD %>% dplyr::filter(location_name=='Global' & sex_name == 'Female')

GBD_Global_Male_n <- reshape2::dcast(data = GBD_Global_Male, 
                                     year~ age_name,
                                     value.var = c("val"),
                                     fun.aggregate = sum)
rownames(GBD_Global_Male_n) <- GBD_Global_Male_n$year
GBD_Global_Male_n <- GBD_Global_Male_n %>% dplyr::select(-1) 
                                     
GBD_Global_Female_n <- reshape2::dcast(data = GBD_Global_Female, 
                                       year ~ age_name,
                                       value.var = c("val"),
                                       fun.aggregate = sum)
rownames(GBD_Global_Female_n) <- GBD_Global_Female_n$year
GBD_Global_Female_n <- GBD_Global_Female_n %>% dplyr::select(-1)

GBD_Global_Both_n <- GBD_Global_Female_n + GBD_Global_Male_n 

# =============================================================================
# 3. Parameter Optimization Framework
# =============================================================================

# Create parameter grid
create_bapc_param_grid <- function() {
  base_params <- expand.grid(
    gf = c(5),
    secondDiff = c(FALSE),
    period_include = c(TRUE, FALSE),
    cohort_include = c(TRUE, FALSE),
    overdis_include = c(TRUE, FALSE),
    age_model = c("rw2"),
    period_model = c("rw2"),
    cohort_model = c("rw2"),
    prior_type = c("loggamma"),
    param_strength = c("default", "conservative"),
    stringsAsFactors = FALSE
  )

  filtered_params <- base_params %>%
    dplyr::filter(period_include == TRUE | cohort_include == TRUE) %>%
    dplyr::filter(!(period_include == FALSE & cohort_include == FALSE))

  return(filtered_params)
}

# Generate expanding window time slices
generate_expanding_slices <- function(start_train_end = 2005, end_train_end = 2015) {
  folds <- list()
  idx <- 0
  for (te in start_train_end:end_train_end) {
    idx <- idx + 1
    folds[[idx]] <- list(train_end = te, valid_years = te + 1)
  }
  folds
}

# Convert parameters to BAPC configuration
convert_to_bapc_config <- function(param_row) {
  param_map <- list(
    default = list(
      loggamma = c(1, 0.0001),
      overdis_loggamma = c(1, 0.01)
    ),
    conservative = list(
      loggamma = c(1, 0.00001),
      overdis_loggamma = c(1, 0.001)
    )
  )

  strength <- param_map[[param_row$param_strength]]
  prior_param <- strength$loggamma
  overdis_param <- strength$overdis_loggamma

  model_config <- list(
    age = list(
      model = param_row$age_model,
      prior = param_row$prior_type,
      param = prior_param,
      initial = 3,
      scale.model = FALSE
    ),
    period = list(
      include = param_row$period_include,
      model = param_row$period_model,
      prior = param_row$prior_type,
      param = prior_param,
      initial = 3,
      scale.model = FALSE
    ),
    cohort = list(
      include = param_row$cohort_include,
      model = param_row$cohort_model,
      prior = param_row$prior_type,
      param = prior_param,
      initial = 3,
      scale.model = FALSE
    ),
    overdis = list(
      include = param_row$overdis_include,
      model = "iid",
      prior = "loggamma",
      param = overdis_param,
      initial = 3
    )
  )

  return(list(
    gf = param_row$gf,
    model = model_config,
    secondDiff = param_row$secondDiff
  ))
}

# Performance evaluation function
evaluate_bapc_performance <- function(bapc_result, validation_data, pathogen_name, validation_years) {
  tryCatch({
    if("ASR" %in% names(bapc_result)) {
      predicted_asr <- bapc_result$ASR %>% as.data.frame() * 10^5
    } else {
      predicted_asr <- agestd.rate(bapc_result) %>% as.data.frame() * 10^5
    }
    
    predicted_asr$year <- as.numeric(rownames(predicted_asr))
    
    available_pred_years <- predicted_asr$year[predicted_asr$year %in% validation_years]
    
    if(length(available_pred_years) == 0) {
      max_year <- max(predicted_asr$year)
      temp_years <- (max_year - length(validation_years) + 1):max_year
      validation_years_adj <- temp_years
    } else {
      validation_years_adj <- validation_years
    }
    
    predicted_validation <- predicted_asr %>%
      dplyr::filter(year %in% validation_years_adj) %>%
      dplyr::select(year, mean) %>%
      rename(predicted_rate = mean)
    
    pathogen_data <- validation_data %>% dplyr::filter(
      pathogen == pathogen_name,
      location == "Global",
      sex == "Both sexes",
      counterfactual == "Associated",
      measure == "Deaths"
    )
    
    age_std_data <- pathogen_data %>%
      dplyr::filter(age == "Age-standardized") %>%
      dplyr::filter(metric == "Rate (per 100,000)")

    if(nrow(age_std_data) == 0) {
      age_specific_data <- pathogen_data %>%
        dplyr::filter(age %in% ages &
                      metric == "Rate (per 100,000)" &
                      year %in% validation_years_adj)

      if(nrow(age_specific_data) == 0) {
        pathogen_data_clean <- pathogen_data %>%
          mutate(age_clean = gsub(" years$", "", age))
        
        ages_clean <- gsub(" years$", "", ages)
        
        age_specific_data <- pathogen_data_clean %>%
          dplyr::filter(age_clean %in% ages_clean &
                       metric == "Rate (per 100,000)" &
                       year %in% validation_years_adj)
                        
        if(nrow(age_specific_data) > 0) {
          age_specific_data <- age_specific_data %>%
            dplyr::mutate(age = age_clean)
          ages_for_match <- ages_clean
        } else {
          ages_for_match <- ages
        }
      } else {
        ages_for_match <- ages
      }

      if(nrow(age_specific_data) > 0) {
        actual_asr_manual <- age_specific_data %>%
          dplyr::arrange(year, age) %>%
          dplyr::group_by(year) %>%
          dplyr::summarise(actual_rate = sum(val * wstand[match(age, ages_for_match)], na.rm = TRUE), .groups = 'drop')

        if(nrow(actual_asr_manual) > 0) {
          actual_asr <- actual_asr_manual
        } else {
          return(list(rmse = NA, mae = NA, n_obs = 0))
        }
      } else {
        return(list(rmse = NA, mae = NA, n_obs = 0))
      }
    } else {
      actual_asr <- age_std_data %>%
        dplyr::filter(year %in% validation_years_adj) %>%
        dplyr::select(year, val) %>%
        rename(actual_rate = val)
    }

    comparison <- merge(predicted_validation, actual_asr, by = "year", all = FALSE)

    if(nrow(comparison) == 0) {
      return(list(rmse = NA, mae = NA, n_obs = 0))
    }

    rmse <- sqrt(mean((comparison$predicted_rate - comparison$actual_rate)^2, na.rm = TRUE))
    mae <- mean(abs(comparison$predicted_rate - comparison$actual_rate), na.rm = TRUE)

    return(list(
      rmse = rmse,
      mae = mae,
      n_obs = nrow(comparison)
    ))

  }, error = function(e) {
    return(list(rmse = NA, mae = NA, n_obs = 0))
  })
}

# Data preprocessing function
prepare_bapc_data <- function(EC_data, ages_vector) {
  EC_incidence <- EC_data %>% 
    dplyr::filter(age %in% ages_vector &
                  sex == 'Both sexes' &
                  metric == 'Number' &
                  measure == 'Deaths' &
                  location == 'Global')
  
  if(nrow(EC_incidence) == 0) return(NULL)
  
  EC_incidence_matrix <- reshape2::dcast(EC_incidence, year~age, value.var = 'val')
  rownames(EC_incidence_matrix) <- EC_incidence_matrix$year
  EC_incidence_matrix <- EC_incidence_matrix[,-1]

  EC_incidence_matrix <- EC_incidence_matrix %>% 
    apply(c(1,2), as.numeric) %>% 
    apply(c(1,2), round) %>% 
    as.data.frame() %>% 
    dplyr::select(all_of(ages_3))
  
  return(EC_incidence_matrix)
}

# Data integrity validation function
validate_data <- function(death_matrix, population_matrix) {
  if(!all(rownames(death_matrix) %in% rownames(population_matrix))) {
    missing_years <- setdiff(rownames(death_matrix), rownames(population_matrix))
    warning("Population data missing years: ", paste(missing_years, collapse = ", "))
    return(FALSE)
  }
  
  if(any(is.na(population_matrix))) {
    warning("Population data contains NA values")
    return(FALSE)
  }
  
  if(any(is.na(death_matrix))) {
    warning("Death data contains NA values")
    return(FALSE)
  }
  
  return(TRUE)
}

# Main optimization function
optimize_bapc_parameters <- function(max_combinations = 30) {
  cat("Starting BAPC parameter optimization for", PATHOGEN_NAME, "...\n")

  param_grid <- create_bapc_param_grid()

  if(nrow(param_grid) > max_combinations) {
    set.seed(123)
    param_grid <- param_grid[sample(nrow(param_grid), max_combinations), ]
  }

  cat("Parameter search space:", nrow(param_grid), "combinations\n\n")

  EC_train_matrix <- prepare_bapc_data(EC_train, ages)
  if(is.null(EC_train_matrix)) {
    stop("Training data is empty, cannot perform optimization")
  }

  train_years <- rownames(EC_train_matrix)
  population_train <- GBD_Global_Both_n[train_years, ]

  if(!validate_data(EC_train_matrix, population_train)) {
    stop("Data validation failed")
  }

  expanding_folds <- generate_expanding_slices()
  optimization_results <- data.frame()

  for(i in 1:nrow(param_grid)) {
    cat("Testing parameter combination", i, "/", nrow(param_grid), "...\n")
    bapc_config <- convert_to_bapc_config(param_grid[i, ])

    fold_metrics <- list()
    for (fi in seq_along(expanding_folds)) {
      fold <- expanding_folds[[fi]]

      EC_train_fold <- EC_train %>% dplyr::filter(year <= fold$train_end)
      EC_train_matrix_fold <- prepare_bapc_data(EC_train_fold, ages)
      if (is.null(EC_train_matrix_fold)) {
        fold_metrics[[fi]] <- list(rmse = NA, mae = NA, n_obs = 0)
        next
      }

      train_years_fold <- rownames(EC_train_matrix_fold)
      extended_years_fold <- c(train_years_fold, as.character(fold$valid_years))
      population_fold <- GBD_Global_Both_n[extended_years_fold, ]
      extended_death_matrix_fold <- matrix(NA, nrow = length(extended_years_fold), ncol = ncol(EC_train_matrix_fold), dimnames = list(extended_years_fold, colnames(EC_train_matrix_fold)))
      extended_death_matrix_fold[train_years_fold, ] <- as.matrix(EC_train_matrix_fold)
      extended_death_matrix_fold <- as.data.frame(extended_death_matrix_fold)

      apc_list_fold <- APCList(extended_death_matrix_fold, population_fold, gf = bapc_config$gf)
      bapc_fold <- BAPC(apc_list_fold, predict = list(npredict = length(as.numeric(fold$valid_years)), retro = FALSE), model = bapc_config$model, secondDiff = bapc_config$secondDiff, stdweight = wstand, verbose = FALSE)
      perf_fold <- evaluate_bapc_performance(bapc_fold, EC2, PATHOGEN_NAME, as.numeric(fold$valid_years))
      fold_metrics[[fi]] <- perf_fold
    }

    rmse_vec <- sapply(fold_metrics, function(x) x$rmse)
    mae_vec <- sapply(fold_metrics, function(x) x$mae)
    n_vec <- sapply(fold_metrics, function(x) x$n_obs)
    agg_rmse <- sqrt(mean(rmse_vec^2, na.rm = TRUE))
    agg_mae <- mean(mae_vec, na.rm = TRUE)

    result_row <- cbind(param_grid[i, ], list(
      rmse = agg_rmse,
      mae = agg_mae,
      n_obs = sum(n_vec, na.rm = TRUE)
    ))
    result_row$param_combination <- i
    optimization_results <- rbind(optimization_results, result_row)
  }

  return(optimization_results)
}

# Model selection function
select_optimal_parameters <- function(optimization_results) {
  if(nrow(optimization_results) == 0) {
    warning("No optimization results, using default parameters")
    return(get_default_parameters())
  }
  
  valid_results <- optimization_results[!is.na(optimization_results$rmse) & !is.na(optimization_results$mae), ]

  if(nrow(valid_results) == 0) {
    warning("No valid parameter combination results, using default parameters")
    return(get_default_parameters())
  }

  cat("\nValid parameter combinations:", nrow(valid_results), "\n")

  rmse_threshold <- median(valid_results$rmse, na.rm = TRUE)
  mae_threshold <- median(valid_results$mae, na.rm = TRUE)

  candidates <- valid_results[valid_results$rmse <= rmse_threshold & 
                             valid_results$mae <= mae_threshold, ]

  if(nrow(candidates) == 0) {
    optimal_idx <- which.min(valid_results$rmse)
    optimal_params <- valid_results[optimal_idx, ]
  } else {
    optimal_idx <- which.min(candidates$rmse)
    optimal_params <- candidates[optimal_idx, ]
  }

  cat("Optimal parameter combination:\n")
  cat("gf:", optimal_params$gf, "secondDiff:", optimal_params$secondDiff, "\n")
  cat("RMSE:", round(optimal_params$rmse, 4), "MAE:", round(optimal_params$mae, 4), "\n")

  return(list(
    optimal_params = optimal_params,
    all_results = valid_results
  ))
}

# Default parameter function
get_default_parameters <- function() {
  default_params <- data.frame(
    gf = 5,
    secondDiff = FALSE,
    period_include = TRUE,
    cohort_include = TRUE,
    overdis_include = TRUE,
    age_model = "rw2",
    period_model = "rw2",
    cohort_model = "rw2",
    prior_type = "loggamma",
    param_strength = "default",
    rmse = NA,
    mae = NA,
    n_obs = 0,
    param_combination = 0,
    stringsAsFactors = FALSE
  )
  
  return(list(
    optimal_params = default_params,
    all_results = default_params
  ))
}

# =============================================================================
# 4. Execute Optimization Workflow
# =============================================================================

cat("========================================\n")
cat("Starting BAPC Parameter Optimization Workflow\n")
cat("========================================\n")

# Execute parameter optimization
optimization_output <- optimize_bapc_parameters(max_combinations = 30)

# Select optimal parameters
selection_result <- select_optimal_parameters(optimization_output)
optimal_params <- selection_result$optimal_params

# =============================================================================
# 5. Final Prediction Using Optimal Parameters
# =============================================================================

cat("Performing final prediction with optimal parameters...\n")
cat("Performing independent hold-out set (2017-2019) validation...\n")
optimal_config <- convert_to_bapc_config(optimal_params)

# Train using 1990-2016
EC_holdout_train <- EC2 %>%
  dplyr::filter(year <= data_splits$train_end &
                counterfactual == "Associated" &
                measure == "Deaths" &
                pathogen == PATHOGEN_NAME) %>%
  standardize_age_labels()

EC_holdout_matrix <- prepare_bapc_data(EC_holdout_train, ages)
if (is.null(EC_holdout_matrix)) {
  stop("Unable to prepare hold-out set training data")
}

holdout_train_years <- rownames(EC_holdout_matrix)
holdout_years <- 2017:2019
extended_years_holdout <- c(holdout_train_years, as.character(holdout_years))
population_holdout <- GBD_Global_Both_n[extended_years_holdout, ]
extended_death_matrix_holdout <- matrix(NA,
                                        nrow = length(extended_years_holdout),
                                        ncol = ncol(EC_holdout_matrix),
                                        dimnames = list(extended_years_holdout, colnames(EC_holdout_matrix)))
extended_death_matrix_holdout[holdout_train_years, ] <- as.matrix(EC_holdout_matrix)
extended_death_matrix_holdout <- as.data.frame(extended_death_matrix_holdout)

apc_list_holdout <- APCList(extended_death_matrix_holdout, population_holdout, gf = optimal_config$gf)
bapc_holdout <- BAPC(
  apc_list_holdout,
  predict = list(npredict = length(holdout_years), retro = FALSE),
  model = optimal_config$model,
  secondDiff = optimal_config$secondDiff,
  stdweight = wstand,
  verbose = FALSE
)

holdout_perf <- evaluate_bapc_performance(bapc_holdout, EC2, PATHOGEN_NAME, holdout_years)
holdout_rmse <- holdout_perf$rmse
holdout_mae <- holdout_perf$mae
cat("Hold-out set (2017-2019) RMSE:", round(holdout_rmse, 4), "MAE:", round(holdout_mae, 4), "\n\n")

# Prepare complete historical data (1990-2019)
EC_full_train <- EC2 %>%
  dplyr::filter(year <= 2019 & 
               counterfactual == "Associated" &
               measure == "Deaths" & 
               pathogen == PATHOGEN_NAME) %>%
  standardize_age_labels()

EC_full_matrix <- prepare_bapc_data(EC_full_train, ages)

if(is.null(EC_full_matrix)) {
  stop("Unable to prepare complete training data")
}

full_years <- rownames(EC_full_matrix)
population_full <- GBD_Global_Both_n[full_years, ]

if(!validate_data(EC_full_matrix, population_full)) {
  stop("Complete data validation failed")
}

# Run final BAPC model
tryCatch({
  prediction_years <- 2020:2021
  
  cat("Training period:", paste(range(as.numeric(full_years)), collapse = "-"), 
      ", prediction period:", paste(range(prediction_years), collapse = "-"), "\n")
  
  extended_years_final <- c(full_years, as.character(prediction_years))
  population_extended_final <- GBD_Global_Both_n[extended_years_final, ]
  
  extended_death_matrix_final <- matrix(NA, 
                                       nrow = length(extended_years_final), 
                                       ncol = ncol(EC_full_matrix),
                                       dimnames = list(extended_years_final, colnames(EC_full_matrix)))
  
  extended_death_matrix_final[full_years, ] <- as.matrix(EC_full_matrix)
  extended_death_matrix_final <- as.data.frame(extended_death_matrix_final)
  
  final_apc_list <- APCList(extended_death_matrix_final, population_extended_final, gf = optimal_config$gf)

  final_bapc_result <- BAPC(
    final_apc_list,
    predict = list(npredict = length(prediction_years), retro = FALSE),
    model = optimal_config$model,
    secondDiff = optimal_config$secondDiff,
    stdweight = wstand,
    verbose = FALSE
  )

  cat("BAPC model training successful!\n")

}, error = function(e) {
  cat("BAPC model failed, error:", e$message, "\n")
  stop("Unable to run final BAPC model")
})

# =============================================================================
# 6. Result Extraction and Visualization
# =============================================================================

# Extract prediction results
tryCatch({
  final_bapc_result <- qapc(final_bapc_result, percentiles=c(0.025,0.975))
  Male_ASR <- agestd.rate(x = final_bapc_result) %>% as.data.frame() * 10^5
  Male_ASR$year <- as.numeric(rownames(Male_ASR))
  
  cat("Result extraction successful!\n")
  
}, error = function(e) {
  cat("Result extraction failed, error:", e$message, "\n")
  stop("Unable to extract prediction results")
})

# Extract actual data for comparison
EC_actual <- EC2 %>% 
  dplyr::filter(counterfactual == "Associated" & 
               measure == "Deaths" & 
             pathogen == PATHOGEN_NAME &
               age == "Age-standardized" &
               metric == "Rate (per 100,000)") %>%
  dplyr::select(year, val) %>%
  rename(actual_rate = val)

# Merge predicted and actual data
predicted_data <- Male_ASR %>%
  dplyr::select(year, mean) %>%
  rename(predicted_rate = mean)

combined_asr <- full_join(EC_actual, predicted_data, by = "year") %>%
  arrange(year)

# Visualize results
ep_data <- combined_asr %>%
  dplyr::filter(year >= 2010 & year <= 2021)

max_value <- max(ep_data$actual_rate, ep_data$predicted_rate, na.rm = TRUE)
upper_limit <- max_value * 1.1

# Create figure
p <- ggplot(ep_data, aes(x = year)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    plot.title = element_text(hjust = 0, size = 12, face = "bold", family = "Arial"),
    axis.title = element_text(size = 10, face = "bold", family = "Arial"),
    axis.text = element_text(size = 9, face = "bold", family = "Arial", color = "black"),
    legend.position = "none",
    plot.margin = margin(t = 15, r = 10, b = 10, l = 15)
  ) +
  geom_line(aes(y = actual_rate), color = "black", linewidth = 1) +
  geom_line(aes(y = predicted_rate), color = "red", linewidth = 1, linetype = "solid") +
  geom_vline(xintercept = 2020, linetype = "dashed", linewidth = 0.8, color = "gray") +
  geom_vline(xintercept = 2021, linetype = "dashed", linewidth = 0.8, color = "gray") +
  labs(
    title = PATHOGEN_NAME,
    x = "Year",
    y = "Age-standardized mortality rate (per 100,000)"
  ) +
  scale_x_continuous(breaks = seq(2010, 2021, 2), limits = c(2010, 2021.5)) +
  scale_y_continuous(limits = c(NA, upper_limit))

print(p)

# =============================================================================
# 7. Calculate ASR Differences and Generate Output Table
# =============================================================================

cat("\n========================================\n")
cat("Calculating ASR Differences for COVID-19 Period\n")
cat("========================================\n")

# Extract predicted data for 2020-2021
covid_years <- c(2020, 2021)
predicted_asr_ci <- Male_ASR %>%
  dplyr::filter(year %in% covid_years) %>%
  dplyr::select(year, mean, `0.025Q`, `0.975Q`) %>%
  rename(predicted_asr = mean, 
         predicted_asr_lower = `0.025Q`,
         predicted_asr_upper = `0.975Q`)

# Extract actual observed data
actual_asr_ci <- EC2 %>%
  dplyr::filter(counterfactual == "Associated" &
                measure == "Deaths" & 
                pathogen == PATHOGEN_NAME &
                age == "Age-standardized" &
                metric == "Rate (per 100,000)" &
                year %in% covid_years) %>%
  dplyr::select(year, val) %>%
  rename(observed_asr = val)

# Merge data
covid_impact_analysis <- predicted_asr_ci %>%
  left_join(actual_asr_ci, by = "year")

# Calculate rate differences with CrI
calculate_rate_differences_cri <- function(observed, predicted_mean, predicted_lower, predicted_upper) {
  ard <- observed - predicted_mean
  ard_lower <- observed - predicted_upper
  ard_upper <- observed - predicted_lower
  rrd <- (observed - predicted_mean) / predicted_mean * 100
  rrd_lower <- (observed - predicted_upper) / predicted_mean * 100
  rrd_upper <- (observed - predicted_lower) / predicted_mean * 100
  return(list(
    ard = ard,
    ard_lower = ard_lower,
    ard_upper = ard_upper,
    rrd = rrd,
    rrd_lower = rrd_lower,
    rrd_upper = rrd_upper
  ))
}

# Calculate ASR differences
asr_differences <- data.frame()
for(i in 1:nrow(covid_impact_analysis)) {
  year <- covid_impact_analysis$year[i]
  observed <- covid_impact_analysis$observed_asr[i]
  predicted <- covid_impact_analysis$predicted_asr[i]
  predicted_lower <- covid_impact_analysis$predicted_asr_lower[i]
  predicted_upper <- covid_impact_analysis$predicted_asr_upper[i]
  
  diff_result <- calculate_rate_differences_cri(observed, predicted, predicted_lower, predicted_upper)
  
  year_result <- data.frame(
    year = year,
    observed_asr = observed,
    predicted_asr = predicted,
    ard_asr = diff_result$ard,
    ard_asr_lower = diff_result$ard_lower,
    ard_asr_upper = diff_result$ard_upper,
    rrd_asr = diff_result$rrd,
    rrd_asr_lower = diff_result$rrd_lower,
    rrd_asr_upper = diff_result$rrd_upper
  )
  
  asr_differences <- rbind(asr_differences, year_result)
}

# Calculate mean differences for 2020-2021
mean_ard_asr <- mean(asr_differences$ard_asr, na.rm = TRUE)
mean_rrd_asr <- mean(asr_differences$rrd_asr, na.rm = TRUE)

# Bayesian summary using posterior sampling
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

set.seed(123)
n_draws <- 10000

asr_ci_ord <- predicted_asr_ci %>% dplyr::arrange(year)
asr_obs_ord <- actual_asr_ci %>% dplyr::arrange(year)

params_asr_2020 <- infer_lognorm_params(asr_ci_ord$predicted_asr_lower[1], asr_ci_ord$predicted_asr_upper[1])
params_asr_2021 <- infer_lognorm_params(asr_ci_ord$predicted_asr_lower[2], asr_ci_ord$predicted_asr_upper[2])

pred_asr_2020_draws <- rlnorm(n_draws, params_asr_2020$meanlog, params_asr_2020$sdlog)
pred_asr_2021_draws <- rlnorm(n_draws, params_asr_2021$meanlog, params_asr_2021$sdlog)

ard_asr_2020_draws <- asr_obs_ord$observed_asr[1] - pred_asr_2020_draws
ard_asr_2021_draws <- asr_obs_ord$observed_asr[2] - pred_asr_2021_draws

rrd_asr_2020_draws <- (asr_obs_ord$observed_asr[1] - pred_asr_2020_draws) / pred_asr_2020_draws * 100
rrd_asr_2021_draws <- (asr_obs_ord$observed_asr[2] - pred_asr_2021_draws) / pred_asr_2021_draws * 100

combined_ard_asr_draws <- (ard_asr_2020_draws + ard_asr_2021_draws) / 2
combined_rrd_asr_draws <- (rrd_asr_2020_draws + rrd_asr_2021_draws) / 2

combined_ard_asr_lower <- as.numeric(quantile(combined_ard_asr_draws, 0.025, na.rm = TRUE))
combined_ard_asr_upper <- as.numeric(quantile(combined_ard_asr_draws, 0.975, na.rm = TRUE))
combined_rrd_asr_lower <- as.numeric(quantile(combined_rrd_asr_draws, 0.025, na.rm = TRUE))
combined_rrd_asr_upper <- as.numeric(quantile(combined_rrd_asr_draws, 0.975, na.rm = TRUE))

# Generate output table with period summary
rmse_val <- suppressWarnings(as.numeric(holdout_rmse))
mae_val <- suppressWarnings(as.numeric(holdout_mae))
rep_len <- length(covid_impact_analysis$year)

asr_with_period <- data.frame(
  RMSE = rep(sprintf("%.4f", rmse_val), rep_len),
  MAE = rep(sprintf("%.4f", mae_val), rep_len),
  Periods = covid_impact_analysis$year,
  Observed = sprintf("%.2f", asr_differences$observed_asr),
  Predicted = sprintf("%.2f", asr_differences$predicted_asr),
  check.names = FALSE
)
asr_with_period$`Absolute difference (95%CI)` <- sprintf("%.2f (%.2f, %.2f)",
                                                       asr_differences$ard_asr,
                                                       asr_differences$ard_asr_lower,
                                                       asr_differences$ard_asr_upper)
asr_with_period$`Relative change (95%CI)` <- sprintf("%.2f%% (%.2f%%, %.2f%%)",
                                                    asr_differences$rrd_asr,
                                                    asr_differences$rrd_asr_lower,
                                                    asr_differences$rrd_asr_upper)

# 2020-2021 summary row
observed_avg_asr <- mean(as.numeric(covid_impact_analysis$observed_asr), na.rm = TRUE)
predicted_avg_asr <- mean(as.numeric(covid_impact_analysis$predicted_asr), na.rm = TRUE)
summary_row <- data.frame(
  RMSE = sprintf("%.4f", rmse_val),
  MAE = sprintf("%.4f", mae_val),
  Periods = "2020-2021",
  Observed = sprintf("%.2f", observed_avg_asr),
  Predicted = sprintf("%.2f", predicted_avg_asr),
  check.names = FALSE
)
summary_row$`Absolute difference (95%CI)` <- sprintf("%.2f (%.2f, %.2f)",
                                                  mean_ard_asr,
                                                  combined_ard_asr_lower,
                                                  combined_ard_asr_upper)
summary_row$`Relative change (95%CI)` <- sprintf("%.2f%% (%.2f%%, %.2f%%)",
                                               mean_rrd_asr,
                                               combined_rrd_asr_lower,
                                               combined_rrd_asr_upper)
asr_with_period <- rbind(asr_with_period, summary_row)

# =============================================================================
# 8. Save Results
# =============================================================================

# Save ASR differences table
write.csv(asr_with_period, paste0(gsub(" ", "_", PATHOGEN_NAME), "_asr_differences_with_2020_2021.csv"), row.names = FALSE)

# Save optimization parameters
param_filename <- paste0(gsub(" ", "_", PATHOGEN_NAME), "_optimal_parameters.csv")
if(nrow(selection_result$all_results) > 0) {
  write.csv(selection_result$all_results, param_filename, row.names = FALSE)
  cat("Parameter optimization results saved:", param_filename, "\n")
} else {
  cat("Used default parameters\n")
}

cat("\n========================================\n")
cat("Analysis Complete!\n")
cat("Pathogen:", PATHOGEN_NAME, "\n")
cat("Optimal parameters - gf:", optimal_params$gf, "secondDiff:", optimal_params$secondDiff, "\n")
if(!is.na(optimal_params$rmse)) {
  cat("Validation performance - RMSE:", round(optimal_params$rmse, 4), "MAE:", round(optimal_params$mae, 4), "\n")
} else {
  cat("Used default parameters\n")
}
cat("Results saved to:", paste0(gsub(" ", "_", PATHOGEN_NAME), "_asr_differences_with_2020_2021.csv"), "\n")
cat("Parameters saved to:", param_filename, "\n")
cat("Analysis completion time:", Sys.time(), "\n")
cat("========================================\n")
