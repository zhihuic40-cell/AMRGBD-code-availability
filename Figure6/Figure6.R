library(BAPC)
library(INLA)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(epitools)
library(reshape2)
getwd()

# Set the pathogen to analyze
PATHOGEN_NAME <- 'Serratia spp.'

# =============================================================================
# 1. Data preparation and splitting strategy
# =============================================================================

# Helper: build train/validation/prediction splits
prepare_data_splits <- function(EC_data, train_end = 2016, validation_end = 2019) {
  return(list(
    training_period = 1990:train_end,
    validation_period = (train_end + 1):validation_end,
    prediction_period = (validation_end + 1):2021,
    train_end = train_end,
    validation_end = validation_end
  ))
}

# Set up data splits
data_splits <- prepare_data_splits(EC_data = NULL, train_end = 2016, validation_end = 2019)
cat("Splitting strategy:\n")
cat("Training:", paste(data_splits$training_period[1], "-", max(data_splits$training_period)), "\n")
cat("Validation:", paste(min(data_splits$validation_period), "-", max(data_splits$validation_period)), "\n")
cat("Prediction:", paste(min(data_splits$prediction_period), "-", max(data_splits$prediction_period)), "\n\n")

EC <-  read.csv('figure6data.csv')
EC2 <- EC
EC1 <- EC %>% dplyr::filter(counterfactual == "Associated" & measure == "Deaths"& pathogen== PATHOGEN_NAME)

# Update filtering to follow splitting strategy
EC_full <- EC %>% dplyr::filter(counterfactual == "Associated" & measure == "Deaths"& pathogen== PATHOGEN_NAME)
EC_train <- EC_full %>% dplyr::filter(year <= data_splits$train_end)
EC_validation <- EC_full %>% dplyr::filter(year > data_splits$train_end & year <= data_splits$validation_end)

age_stand <- read.csv('GBD2021 world population age standard.csv')[-c(1:3),]
rownames(age_stand) <- 1:nrow(age_stand)

# Function: standardize age labels
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

# Define age structures
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

# Compute standard weights
wstand <- c(age_stand$std_population[1:2] %>% as.numeric() %>% sum(),
            age_stand$std_population[3:21] %>% as.numeric())/sum(age_stand$std_population[1:21])

# =============================================================================
# 2. Population data processing (original logic preserved)
# =============================================================================

# [Population data processing code unchanged]
dirname <- dir("GBD_Population")
file <- paste0(getwd(),"/GBD_Population/",dirname)
var_name <- c('location_id',"location_name","sex_name","year","age_name","val")

GBD_population  <-  as.data.frame(matrix(nrow=0,ncol=length(var_name)))
names(GBD_population)=var_name
for (a in file) {
  data <- fread(a) %>% as.data.frame() %>% dplyr::select(var_name) %>% 
    dplyr::filter(age_name %in% ages_2 & location_id !=533 & year <= 2019)  # limit historical data to 2019
  GBD_population <- rbind(GBD_population,data)
}

GBD_population <- GBD_population %>% dplyr::select(-1)

# Merge 12–23 months and 2–4 years into a 2–4 years group
GBD_2to4 <- GBD_population %>% 
  dplyr::filter(age_name %in% c("12-23 months", "2-4 years")) %>%
  group_by(location_name, sex_name, year) %>% 
  summarise(val = sum(val)) %>%
  mutate(age_name = "2-4 years") %>% 
  dplyr::select(var_name[-1])

GBD_population <- GBD_population %>% 
  dplyr::filter(!(age_name %in% c("12-23 months", "2-4 years")))
GBD_population <- rbind(GBD_population, GBD_2to4)

# Predicted population data
prediction_var_name <- c("location_name","sex","year_id","age_group_name","val")
# Fix: only use 2020+ for projection to avoid overlap with history
GBD_population_prediction <- fread('IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.CSV') %>%
  dplyr::select(prediction_var_name) %>%
  dplyr::filter(year_id %in% 2020:2030)  # start from 2020 to avoid overlap
names(GBD_population_prediction) <- var_name[-1]

# Combine neonatal groups into <1 year
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

# Combine population data
GBD <- rbind(GBD_population,GBD_population_prediction)

# Print ranges for sanity check
cat("Historical population years:", paste(range(GBD_population$year), collapse = " - "), "\n")
cat("Projected population years:", paste(range(GBD_population_prediction$year), collapse = " - "), "\n")
cat("Combined population years:", paste(range(GBD$year), collapse = " - "), "\n")

# Build 0–4 years age group
GBD_age4 <- GBD %>% dplyr::filter(age_name %in% c("<1 year","2-4 years")) %>%
  group_by(location_name,sex_name,year) %>%
  summarize(val=sum(val)) %>%  mutate(age_name='0-4 years') %>%
  dplyr::select(var_name[-1])
GBD <- rbind(GBD,GBD_age4)

GBD <- subset(GBD, age_name %in% ages_3) %>%
  mutate(age_name=factor(age_name,levels=ages_3,ordered = T)) %>%
  arrange(age_name)

# Extract global population by sex
GBD_Global_Male <- GBD %>% dplyr::filter(location_name=='Global' & sex_name == 'Male')
GBD_Global_Female <-  GBD %>% dplyr::filter(location_name=='Global' & sex_name == 'Female')

GBD_Global_Male_n <- reshape2::dcast(data = GBD_Global_Male, 
                                     year~ age_name,
                                     value.var = c("val"),
                                     fun.aggregate = sum)
# Set year as rownames and drop the year column
rownames(GBD_Global_Male_n) <- GBD_Global_Male_n$year
GBD_Global_Male_n <- GBD_Global_Male_n %>% dplyr::select(-1) 
                                     
GBD_Global_Female_n <- reshape2::dcast(data = GBD_Global_Female, 
                                       year ~ age_name,
                                       value.var = c("val"),
                                       fun.aggregate = sum)
# Set year as rownames and drop the year column  
rownames(GBD_Global_Female_n) <- GBD_Global_Female_n$year
GBD_Global_Female_n <- GBD_Global_Female_n %>% dplyr::select(-1)

GBD_Global_Both_n <- GBD_Global_Female_n + GBD_Global_Male_n 

cat("\n=== Diagnostics: GBD_Global_Both_n matrix ===\n")
cat("Dimensions:", dim(GBD_Global_Both_n), "\n")
cat("First 10 rownames:", paste(head(rownames(GBD_Global_Both_n), 10), collapse = ", "), "\n")
cat("Last 10 rownames:", paste(tail(rownames(GBD_Global_Both_n), 10), collapse = ", "), "\n")
cat("Row year range:", paste(range(as.numeric(rownames(GBD_Global_Both_n))), collapse = " - "), "\n")
cat("Contains 1990–2016:", all(1990:2016 %in% rownames(GBD_Global_Both_n)), "\n")
cat("Missing training years:", paste(setdiff(1990:2016, rownames(GBD_Global_Both_n)), collapse = ", "), "\n")
cat("===============================\n\n")

# =============================================================================
# 3. Parameter optimization framework
# =============================================================================

# Create a stable parameter grid (mitigate INLA crashes)
create_bapc_param_grid <- function() {
  # Simplify the grid and prioritize stability
  base_params <- expand.grid(
    # Use a conservative gf range (start with doc default)
    gf = c(5),
    
    # Use FALSE only to avoid numerical issues
    secondDiff = c(FALSE),
    
    # Simplify model combinations
    period_include = c(TRUE, FALSE),
    cohort_include = c(TRUE, FALSE),
    overdis_include = c(TRUE, FALSE),
    
    # Use RW2 only for stability
    age_model = c("rw2"),
    period_model = c("rw2"),
    cohort_model = c("rw2"),
    
    # Use loggamma priors only
    prior_type = c("loggamma"),
    
    # Conservative vs default settings
    param_strength = c("default", "conservative"),
    
    stringsAsFactors = FALSE
  )

  # Stricter filtering
  filtered_params <- base_params %>%
    # Must include at least one time effect
    dplyr::filter(period_include == TRUE | cohort_include == TRUE) %>%
    # Ensure model identifiability
    dplyr::filter(!(period_include == FALSE & cohort_include == FALSE))

  return(filtered_params)
}

# Build expanding-window folds within 1990–2016
generate_expanding_slices <- function(start_train_end = 2005, end_train_end = 2015) {
  folds <- list()
  idx <- 0
  for (te in start_train_end:end_train_end) {
    idx <- idx + 1
    folds[[idx]] <- list(train_end = te, valid_years = te + 1)
  }
  folds
}

# Convert a grid row to BAPC config with stable priors
convert_to_bapc_config <- function(param_row) {
  # 使用更稳定的先验参数
  param_map <- list(
    # Defaults from docs but slightly more conservative
    default = list(
      loggamma = c(1, 0.0001),        # slightly larger variance for stability
      overdis_loggamma = c(1, 0.01)   # moderately conservative
    ),
    # Conservative (more smoothing, more stability)
    conservative = list(
      loggamma = c(1, 0.00001),       # smaller variance = more smoothing
      overdis_loggamma = c(1, 0.001)
    )
  )

  strength <- param_map[[param_row$param_strength]]
  prior_param <- strength$loggamma
  overdis_param <- strength$overdis_loggamma

  # Build model config following BAPC docs
  model_config <- list(
    # Age effect: always included; typically RW2
    age = list(
      model = param_row$age_model,
      prior = param_row$prior_type,
      param = prior_param,
      initial = 3,                     # lower initial value for stability
      scale.model = FALSE
    ),
    
    # Period effect: optional
    period = list(
      include = param_row$period_include,
      model = param_row$period_model,
      prior = param_row$prior_type,
      param = prior_param,
      initial = 3,
      scale.model = FALSE
    ),
    
    # Cohort effect: optional
    cohort = list(
      include = param_row$cohort_include,
      model = param_row$cohort_model,
      prior = param_row$prior_type,
      param = prior_param,
      initial = 3,
      scale.model = FALSE
    ),
    
    # Overdispersion: iid with separate priors
    overdis = list(
      include = param_row$overdis_include,
      model = "iid",
      prior = "loggamma",
      param = overdis_param,
      initial = 3                      # lower initial value
    )
  )

  return(list(
    gf = param_row$gf,
    model = model_config,
    secondDiff = param_row$secondDiff
  ))
}

# Performance evaluation (robust)
evaluate_bapc_performance <- function(bapc_result, validation_data, pathogen_name, validation_years) {
  tryCatch({
    cat("    [Debug] Starting performance evaluation...\n")
    
    # Inspect BAPC result structure
    cat("    [Debug] BAPC result class:", class(bapc_result), "\n")
    cat("    [Debug] BAPC result names:", names(bapc_result), "\n")
    
    # Extract predicted age-standardised rate; handle year alignment
    if("ASR" %in% names(bapc_result)) {
      # Use ASR field when present
      predicted_asr <- bapc_result$ASR %>% as.data.frame() * 10^5
    } else {
      # Otherwise compute with agestd.rate
      predicted_asr <- agestd.rate(bapc_result) %>% as.data.frame() * 10^5
    }
    
    predicted_asr$year <- as.numeric(rownames(predicted_asr))
    
    cat("    [Debug] Predicted ASR dims:", dim(predicted_asr), "\n")
    cat("    [Debug] Predicted year range:", paste(range(predicted_asr$year), collapse = " - "), "\n")
    cat("    [Debug] Predicted ASR cols:", paste(names(predicted_asr), collapse = ", "), "\n")
    cat("    [Debug] Validation years:", paste(validation_years, collapse = ", "), "\n")
    
    # Check whether validation years are included
    available_pred_years <- predicted_asr$year[predicted_asr$year %in% validation_years]
    cat("    [Debug] Available predicted validation years:", paste(available_pred_years, collapse = ", "), "\n")
    
    # If missing, adjust to available predicted range
    if(length(available_pred_years) == 0) {
      cat("    [Debug] Warning: no validation years in predictions; checking range...\n")
      
      # 尝试从BAPC结果中获取预测年份
      if("predict" %in% names(bapc_result)) {
        cat("    [Debug] BAPC has a predict field\n")
        # 可能需要特殊处理预测结果
      }
      
      # Fallback: use trailing years for evaluation
      max_year <- max(predicted_asr$year)
      temp_years <- (max_year - length(validation_years) + 1):max_year
      cat("    [Debug] Using fallback years:", paste(temp_years, collapse = ", "), "\n")
      validation_years_adj <- temp_years
    } else {
      validation_years_adj <- validation_years
    }
    
    # Subset to validation years
    predicted_validation <- predicted_asr %>%
      dplyr::filter(year %in% validation_years_adj) %>%
      dplyr::select(year, mean) %>%
      rename(predicted_rate = mean)
    
    cat("    [Debug] Rows in predicted validation:", nrow(predicted_validation), "\n")
    if(nrow(predicted_validation) > 0) {
      cat("    [Debug] Validation years in predicted:", paste(predicted_validation$year, collapse = ", "), "\n")
      cat("    [Debug] Predicted value range:", paste(round(range(predicted_validation$predicted_rate), 2), collapse = " - "), "\n")
    }

    # Inspect validation data
    cat("    [Debug] Validation data dims:", dim(validation_data), "\n")
    cat("    [Debug] Validation data cols:", paste(names(validation_data), collapse = ", "), "\n")
    
    # Check pathogen in validation data
    available_pathogens <- unique(validation_data$pathogen)
    cat("    [Debug] Example pathogens:", paste(head(available_pathogens, 3), collapse = ", "), "...\n")
    cat("    [Debug] Target pathogen:", pathogen_name, "\n")
    cat("    [Debug] Pathogen present:", pathogen_name %in% available_pathogens, "\n")
    
    # Pathogen-specific subset
    pathogen_data <- validation_data %>% dplyr::filter(
      pathogen == pathogen_name,
      location == "Global",
      sex == "Both sexes",
      counterfactual == "Associated",
      measure == "Deaths"
    )
    cat("    [Debug] Rows in pathogen subset:", nrow(pathogen_data), "\n")
    if(nrow(pathogen_data) > 0) {
      cat("    [Debug] Years:", paste(sort(unique(pathogen_data$year)), collapse = ", "), "\n")
      cat("    [Debug] Age values (head):", paste(head(unique(pathogen_data$age), 5), collapse = ", "), "...\n")
      cat("    [Debug] Metric values:", paste(unique(pathogen_data$metric), collapse = ", "), "\n")
    }
    
    # Check if age-standardised data exists
    age_std_data <- pathogen_data %>%
      dplyr::filter(age == "Age-standardized") %>%
      dplyr::filter(metric == "Rate (per 100,000)")

    cat("    [Debug] Rows in age-standardised subset:", nrow(age_std_data), "\n")

    # If not, compute manually
    if(nrow(age_std_data) == 0) {
      cat("    [Debug] No ready-made ASR; attempting manual calculation...\n")

      # Check age-specific rates and align age labels
      cat("    [Debug] Target ages (head):", paste(head(ages, 3), collapse = ", "), "...\n")
      
      age_specific_data <- pathogen_data %>%
        dplyr::filter(age %in% ages &
                      metric == "Rate (per 100,000)" &
                      year %in% validation_years_adj)

      cat("    [Debug] Rows in age-specific subset:", nrow(age_specific_data), "\n")
      
      # 如果仍然匹配不到，尝试处理年龄字段格式差异
      if(nrow(age_specific_data) == 0) {
        cat("    [Debug] Trying to fix age label formatting...\n")
        
        # Show actual age labels in validation data
        unique_ages <- unique(pathogen_data$age)
        cat("    [Debug] Actual age labels:", paste(head(unique_ages, 8), collapse = ", "), "...\n")
        
        # 尝试去掉" years"后缀进行匹配
        pathogen_data_clean <- pathogen_data %>%
          mutate(age_clean = gsub(" years$", "", age))
        
        ages_clean <- gsub(" years$", "", ages)
        cat("    [Debug] Cleaned age labels:", paste(head(ages_clean, 3), collapse = ", "), "...\n")
        
        age_specific_data <- pathogen_data_clean %>%
          dplyr::filter(age_clean %in% ages_clean &
                       metric == "Rate (per 100,000)" &
                       year %in% validation_years_adj)
                        
        cat("    [Debug] Rows after label fix:", nrow(age_specific_data), "\n")
        
        if(nrow(age_specific_data) > 0) {
          # 使用清理后的年龄进行匹配
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
        cat("    [调试] 可用验证年份:", paste(sort(unique(age_specific_data$year)), collapse = ", "), "\n")
        cat("    [调试] 可用年龄组:", paste(sort(unique(age_specific_data$age)), collapse = ", "), "\n")

        # Manually compute age-standardised rate
        actual_asr_manual <- age_specific_data %>%
          dplyr::arrange(year, age) %>%
          dplyr::group_by(year) %>%
          dplyr::summarise(actual_rate = sum(val * wstand[match(age, ages_for_match)], na.rm = TRUE), .groups = 'drop')

        cat("    [Debug] Manual ASR rows:", nrow(actual_asr_manual), "\n")

        if(nrow(actual_asr_manual) > 0) {
          actual_asr <- actual_asr_manual
          cat("    [Debug] Using manual ASR\n")
        } else {
          cat("    [Debug] Manual ASR failed\n")
          return(list(rmse = NA, mae = NA, n_obs = 0))
        }
      } else {
        cat("    [Debug] No age-specific validation data\n")
        return(list(rmse = NA, mae = NA, n_obs = 0))
      }
    } else {
      # Use available age-standardised data
      actual_asr <- age_std_data %>%
        dplyr::filter(year %in% validation_years_adj) %>%
        dplyr::select(year, val) %>%
        rename(actual_rate = val)
      cat("    [Debug] Using existing ASR\n")
    }

    cat("    [Debug] Rows in actual validation:", nrow(actual_asr), "\n")
    if(nrow(actual_asr) > 0) {
      cat("    [Debug] Actual years:", paste(actual_asr$year, collapse = ", "), "\n")
      cat("    [Debug] Actual value range:", paste(round(range(actual_asr$actual_rate), 2), collapse = " - "), "\n")
    }

    # Merge predicted and actual
    comparison <- merge(predicted_validation, actual_asr, by = "year", all = FALSE)
    cat("    [Debug] Rows after merge:", nrow(comparison), "\n")

    if(nrow(comparison) == 0) {
      cat("    [Debug] Warning: could not match validation years\n")
      return(list(rmse = NA, mae = NA, n_obs = 0))
    }

    cat("    [Debug] Matched years:", paste(comparison$year, collapse = ", "), "\n")
    cat("    [Debug] Predicted values:", paste(round(comparison$predicted_rate, 2), collapse = ", "), "\n")
    cat("    [Debug] Actual values:", paste(round(comparison$actual_rate, 2), collapse = ", "), "\n")

    # 计算评估指标
    rmse <- sqrt(mean((comparison$predicted_rate - comparison$actual_rate)^2, na.rm = TRUE))
    mae <- mean(abs(comparison$predicted_rate - comparison$actual_rate), na.rm = TRUE)

    cat("    [Debug] Done - RMSE:", round(rmse, 4), "MAE:", round(mae, 4), "\n")

    return(list(
      rmse = rmse,
      mae = mae,
      n_obs = nrow(comparison)
    ))

  }, error = function(e) {
    cat("    [Debug] Performance evaluation error:", e$message, "\n")
    return(list(rmse = NA, mae = NA, n_obs = 0))
  })
}

# Data preprocessing helper (enhanced)
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

# Validate consistency between death and population matrices
validate_data <- function(death_matrix, population_matrix) {
  # Check rowname alignment
  if(!all(rownames(death_matrix) %in% rownames(population_matrix))) {
    missing_years <- setdiff(rownames(death_matrix), rownames(population_matrix))
    warning("Missing years in population data: ", paste(missing_years, collapse = ", "))
    return(FALSE)
  }
  
  # Check for NA values
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

# Main optimization routine (fixed)
optimize_bapc_parameters <- function(max_combinations = 30) {
  cat("Starting BAPC parameter optimization for", PATHOGEN_NAME, "...\n")

  # Generate parameter grid
  param_grid <- create_bapc_param_grid()

  # Limit search space
  if(nrow(param_grid) > max_combinations) {
    set.seed(123)  # reproducible
    param_grid <- param_grid[sample(nrow(param_grid), max_combinations), ]
  }

  cat("Search space:", nrow(param_grid), "combinations\n\n")

  # Prepare training data
  EC_train_matrix <- prepare_bapc_data(EC_train, ages)
  if(is.null(EC_train_matrix)) {
    stop("Training data is empty; cannot optimize")
  }

  # Diagnostics for training data quality
  cat("=== Training data quality diagnostics ===\n")
  cat("Matrix dims:", dim(EC_train_matrix), "\n")
  cat("Year range:", paste(range(as.numeric(rownames(EC_train_matrix))), collapse = " - "), "\n")
  cat("Total deaths:", sum(EC_train_matrix, na.rm = TRUE), "\n")
  cat("Zero fraction:", round(sum(EC_train_matrix == 0, na.rm = TRUE) / length(EC_train_matrix) * 100, 2), "%\n")
  cat("NA count:", sum(is.na(EC_train_matrix)), "\n")
  cat("Min:", min(EC_train_matrix, na.rm = TRUE), "\n")
  cat("Max:", max(EC_train_matrix, na.rm = TRUE), "\n")
  cat("Median:", median(as.matrix(EC_train_matrix), na.rm = TRUE), "\n")
  
  # Check sparsity
  non_zero_cols <- colSums(EC_train_matrix > 0, na.rm = TRUE)
  cat("Non-zero years per age group:", paste(non_zero_cols, collapse = ", "), "\n")
  sparse_age_groups <- names(non_zero_cols)[non_zero_cols < 10]
  if(length(sparse_age_groups) > 0) {
    cat("Warning: sparse age groups:", paste(sparse_age_groups, collapse = ", "), "\n")
  }
  cat("========================\n\n")

  # Align population for the training period
  train_years <- rownames(EC_train_matrix)
  population_train <- GBD_Global_Both_n[train_years, ]

  # Validate matrices
  if(!validate_data(EC_train_matrix, population_train)) {
    stop("Data validation failed")
  }

  expanding_folds <- generate_expanding_slices()
  optimization_results <- data.frame()

  for(i in 1:nrow(param_grid)) {
    cat("Testing param set", i, "/", nrow(param_grid), "...\n")
    bapc_config <- convert_to_bapc_config(param_grid[i, ])

    fold_metrics <- list()
    for (fi in seq_along(expanding_folds)) {
      fold <- expanding_folds[[fi]]
      cat("  [Expanding window] train ≤", fold$train_end, "→ predict ", paste(fold$valid_years, collapse=","), "\n")

      # Build matrices for this fold
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
      cat("    Fold RMSE:", round(perf_fold$rmse,4), "MAE:", round(perf_fold$mae,4), "\n")
    }

    # Aggregate fold metrics (single-year validation)
    rmse_vec <- sapply(fold_metrics, function(x) x$rmse)
    mae_vec <- sapply(fold_metrics, function(x) x$mae)
    n_vec <- sapply(fold_metrics, function(x) x$n_obs)
    # Robust aggregation: RMSE from mean squared error; MAE averaged
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

# Model selection (fixed)
select_optimal_parameters <- function(optimization_results) {
  # Handle empty results
  if(nrow(optimization_results) == 0) {
    warning("No optimization results; using defaults")
    return(get_default_parameters())
  }
  
  # Remove failed combinations
  valid_results <- optimization_results[!is.na(optimization_results$rmse) & !is.na(optimization_results$mae), ]

  if(nrow(valid_results) == 0) {
    warning("No valid combinations; using defaults")
    return(get_default_parameters())
  }

  cat("\nValid parameter sets:", nrow(valid_results), "\n")

  # Dual-criterion selection
  rmse_threshold <- median(valid_results$rmse, na.rm = TRUE)
  mae_threshold <- median(valid_results$mae, na.rm = TRUE)

  # Filter candidates
  candidates <- valid_results[valid_results$rmse <= rmse_threshold & 
                             valid_results$mae <= mae_threshold, ]

  if(nrow(candidates) == 0) {
    # Fallback to global best
    optimal_idx <- which.min(valid_results$rmse)
    optimal_params <- valid_results[optimal_idx, ]
    cat("Fallback to global RMSE best\n")
  } else {
    # Choose min RMSE among candidates
    optimal_idx <- which.min(candidates$rmse)
    optimal_params <- candidates[optimal_idx, ]
    cat("Selected dual-criterion best\n")
  }

  cat("Best parameter set:\n")
  cat("gf:", optimal_params$gf, "secondDiff:", optimal_params$secondDiff, "\n")
  cat("RMSE:", round(optimal_params$rmse, 4), "MAE:", round(optimal_params$mae, 4), "\n")

  return(list(
    optimal_params = optimal_params,
    all_results = valid_results
  ))
}

# Default parameters (per BAPC docs)
get_default_parameters <- function() {
  default_params <- data.frame(
    # Use recommended settings from docs
    gf = 5,                          # example uses 5
    secondDiff = FALSE,              # default FALSE
    period_include = TRUE,           # classic APC includes all effects
    cohort_include = TRUE,
    overdis_include = TRUE,
    age_model = "rw2",              # default
    period_model = "rw2",           # default
    cohort_model = "rw2",           # default
    prior_type = "loggamma",        # default
    param_strength = "default",     # default strength
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
# 4. Run optimization workflow  
# =============================================================================

cat("========================================\n")
cat("Starting BAPC optimization workflow\n")
cat("========================================\n")

# 执行参数优化
optimization_output <- optimize_bapc_parameters(max_combinations = 30)

# Select the optimal parameters
selection_result <- select_optimal_parameters(optimization_output)
optimal_params <- selection_result$optimal_params

# =============================================================================
# 5. Final prediction with optimal parameters
# =============================================================================

cat("Running final prediction using optimal parameters...\n")
# Before final training, perform a one-off 2017–2019 holdout check
cat("Performing independent holdout check (2017–2019)...\n")
optimal_config <- convert_to_bapc_config(optimal_params)

# Train using 1990–2016
EC_holdout_train <- EC2 %>%
  dplyr::filter(year <= data_splits$train_end &
                counterfactual == "Associated" &
                measure == "Deaths" &
                pathogen == PATHOGEN_NAME) %>%
  standardize_age_labels()

EC_holdout_matrix <- prepare_bapc_data(EC_holdout_train, ages)
if (is.null(EC_holdout_matrix)) {
  stop("Unable to prepare holdout training data")
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
cat("保留集(2017–2019) RMSE:", round(holdout_rmse, 4), "MAE:", round(holdout_mae, 4), "\n\n")


# 准备完整历史数据 (1990-2019)
EC_full_train <- EC2 %>%
  dplyr::filter(year <= 2019 & 
               counterfactual == "Associated" &
               measure == "Deaths" & 
               pathogen == PATHOGEN_NAME) %>%
  standardize_age_labels()

EC_full_matrix <- prepare_bapc_data(EC_full_train, ages)

if(is.null(EC_full_matrix)) {
  stop("无法准备完整训练数据")
}

# Align population for full training years
full_years <- rownames(EC_full_matrix)
population_full <- GBD_Global_Both_n[full_years, ]

# 验证完整数据
if(!validate_data(EC_full_matrix, population_full)) {
  stop("Full data validation failed")
}

# 转换最优参数（已在保留集评估中取得optimal_config）

# Run final BAPC model (train 1990–2019; predict 2020–2021)
tryCatch({
  # Predict up to 2021
  prediction_years <- 2020:2021
  
  cat("Standard BAPC: train ", paste(range(as.numeric(full_years)), collapse = "-"), 
      ", predict ", paste(range(prediction_years), collapse = "-"), "\n")
  
  # Build extended structures (training + prediction periods)
  extended_years_final <- c(full_years, as.character(prediction_years))
  population_extended_final <- GBD_Global_Both_n[extended_years_final, ]
  
  cat("Population (extended) years:", paste(range(as.numeric(rownames(population_extended_final))), collapse = " - "), "\n")
  
  # Create extended death matrix (training filled, prediction NA)
  extended_death_matrix_final <- matrix(NA, 
                                       nrow = length(extended_years_final), 
                                       ncol = ncol(EC_full_matrix),
                                       dimnames = list(extended_years_final, colnames(EC_full_matrix)))
  
  # Fill training data
  extended_death_matrix_final[full_years, ] <- as.matrix(EC_full_matrix)
  extended_death_matrix_final <- as.data.frame(extended_death_matrix_final)
  
  cat("Extended death matrix dims:", dim(extended_death_matrix_final), "\n")
  cat("Extended death matrix years:", paste(range(as.numeric(rownames(extended_death_matrix_final))), collapse = " - "), "\n")
  
  # 创建APCList（现在维度匹配，包含预测期的NA结构）
  final_apc_list <- APCList(extended_death_matrix_final, population_extended_final, gf = optimal_config$gf)

  final_bapc_result <- BAPC(
    final_apc_list,
    predict = list(npredict = length(prediction_years), retro = FALSE),  # 预测2020-2021年
    model = optimal_config$model,
    secondDiff = optimal_config$secondDiff,
    stdweight = wstand,
    verbose = FALSE
  )

  cat("BAPC模型训练成功！预测到", max(prediction_years), "年\n")
  
  # 诊断：检查final_bapc_result的结构和年份范围
  cat("\n=== final_bapc_result 详细诊断 ===\n")
  cat("final_bapc_result 类型:", class(final_bapc_result), "\n")
  cat("final_bapc_result 结构:", str(final_bapc_result, max.level = 1), "\n")
  
  # Inspect components
  all_names <- names(final_bapc_result)
  if(length(all_names) > 0) {
    cat("所有组件名称:", paste(all_names, collapse = ", "), "\n")
    
    for(comp_name in all_names) {
      comp <- final_bapc_result[[comp_name]]
      if(is.matrix(comp) || is.data.frame(comp)) {
        if(!is.null(rownames(comp))) {
          years <- try(as.numeric(rownames(comp)), silent = TRUE)
          if(!inherits(years, "try-error") && !all(is.na(years))) {
            cat(comp_name, "组件年份范围:", paste(range(years, na.rm = TRUE), collapse = " - "), "\n")
            cat(comp_name, "组件维度:", dim(comp), "\n")
          }
        }
      }
    }
  } else {
    cat("Warning: final_bapc_result has no named components!\n")
  }
  
  # Try to directly extract results for diagnostics
  cat("\n=== Attempting direct extraction ===\n")
  
  # Method 1: agespec.proj
  test_proj <- try(agespec.proj(final_bapc_result), silent = TRUE)
  if(!inherits(test_proj, "try-error")) {
    proj_years <- try(as.numeric(rownames(test_proj)), silent = TRUE)
    if(!inherits(proj_years, "try-error")) {
      cat("agespec.proj 年份范围:", paste(range(proj_years, na.rm = TRUE), collapse = " - "), "\n")
      cat("agespec.proj 维度:", dim(test_proj), "\n")
    }
  } else {
    cat("agespec.proj 提取失败:", test_proj, "\n")
  }
  
  # 方法2：检查agestd.rate结果（在qapc之前）
  test_asr_before <- try(agestd.rate(final_bapc_result), silent = TRUE)
  if(!inherits(test_asr_before, "try-error")) {
    asr_years <- try(as.numeric(rownames(test_asr_before)), silent = TRUE)
    if(!inherits(asr_years, "try-error")) {
      cat("agestd.rate (pre-qapc) years:", paste(range(asr_years, na.rm = TRUE), collapse = " - "), "\n")
      cat("agestd.rate (pre-qapc) dims:", dim(test_asr_before), "\n")
    }
  } else {
    cat("agestd.rate (pre-qapc) extraction failed:", test_asr_before, "\n")
  }
  
  cat("================================\n\n")

}, error = function(e) {
  cat("BAPC model failed, error:", e$message, "\n")
  stop("Unable to run final BAPC model")
})

# =============================================================================
# 6. Result extraction and analysis
# =============================================================================
  
  # Extract predictions
tryCatch({
  Male_proj <- agespec.proj(x = final_bapc_result) %>% as.data.frame()
  Male_proj_mean <- Male_proj[,colnames(Male_proj) %like% 'mean']
  names(Male_proj_mean) <- ages_3
  
  cat("\n=== Extraction diagnostics ===\n")
  cat("Male_proj dims:", dim(Male_proj), "\n")
  cat("Male_proj rownames (years):", paste(head(rownames(Male_proj), 5), "...", tail(rownames(Male_proj), 5)), "\n")
  cat("Male_proj year range:", paste(range(as.numeric(rownames(Male_proj))), collapse = " - "), "\n")
  
  # Compute age-standardised mortality and percentiles
  final_bapc_result <- qapc(final_bapc_result, percentiles=c(0.025,0.975))
  Male_ASR <- agestd.rate(x = final_bapc_result) %>% as.data.frame() * 10^5
  Male_ASR$year <- as.numeric(rownames(Male_ASR))
  
  cat("Male_ASR dims:", dim(Male_ASR), "\n")
  cat("Male_ASR rownames (years):", paste(head(rownames(Male_ASR), 5), "...", tail(rownames(Male_ASR), 5)), "\n")
  cat("Male_ASR year range:", paste(range(Male_ASR$year), collapse = " - "), "\n")
  
  # 基于后验分位数的年龄特异性预测（用于死亡总数CrI）
  Male_proj_q <- agespec.proj(x = final_bapc_result) %>% as.data.frame()
  Male_proj_mean <- Male_proj_q[, colnames(Male_proj_q) %like% 'mean']; names(Male_proj_mean) <- ages_3
  Male_proj_lower <- Male_proj_q[, colnames(Male_proj_q) %like% '0.025Q']; names(Male_proj_lower) <- ages_3
  Male_proj_upper <- Male_proj_q[, colnames(Male_proj_q) %like% '0.975Q']; names(Male_proj_upper) <- ages_3

  # Approximate total deaths by summing across ages, including percentiles
  Male_sum_year_mean <- apply(Male_proj_mean, 1, sum) %>% as.data.frame()
  Male_sum_year_lower <- apply(Male_proj_lower, 1, sum) %>% as.data.frame()
  Male_sum_year_upper <- apply(Male_proj_upper, 1, sum) %>% as.data.frame()
  colnames(Male_sum_year_mean) <- 'number'
  colnames(Male_sum_year_lower) <- 'number_lower'
  colnames(Male_sum_year_upper) <- 'number_upper'
  Male_sum_year <- cbind(Male_sum_year_mean, Male_sum_year_lower, Male_sum_year_upper)
  Male_sum_year$year <- as.numeric(rownames(Male_sum_year))
  
  cat("Male_sum_year dims:", dim(Male_sum_year), "\n")
  cat("Male_sum_year year range:", paste(range(Male_sum_year$year), collapse = " - "), "\n")
  cat("====================\n\n")
  
  cat("Extraction succeeded!\n")
  
}, error = function(e) {
  cat("Extraction failed, error:", e$message, "\n")
  stop("Unable to extract prediction results")
})

# Pull observed data for comparison
  EC_actual <- EC2 %>% 
    dplyr::filter(counterfactual == "Associated" & 
                 measure == "Deaths" & 
               pathogen == PATHOGEN_NAME &
                 age == "Age-standardized" &
                 metric == "Rate (per 100,000)") %>%
    dplyr::select(year, val) %>%
    rename(actual_rate = val)
  
# Merge predicted and observed
  predicted_data <- Male_ASR %>%
    dplyr::select(year, mean) %>%
    rename(predicted_rate = mean)
  
  combined_asr <- full_join(EC_actual, predicted_data, by = "year") %>%
    arrange(year)
  
# Visualise results
ep_data <- combined_asr %>%
  dplyr::filter(year >= 2010 & year <= 2021)

  max_value <- max(ep_data$actual_rate, ep_data$predicted_rate, na.rm = TRUE)
  upper_limit <- max_value * 1.1
  
# Build figure for predictions vs observations
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
      y = "Age-standardised mortality rate (per 100k)"
    ) +
    scale_x_continuous(breaks = seq(2010, 2021, 2), limits = c(2010, 2021.5)) +
    scale_y_continuous(limits = c(NA, upper_limit))
  
print(p)

# Build export table
export_table <- data.frame(
  Year = c(2020, 2021),
  stringsAsFactors = FALSE
)

# Add observed data
actual_asr_data <- EC2 %>%
  dplyr::filter(counterfactual == "Associated" &
               measure == "Deaths" & 
               pathogen == PATHOGEN_NAME &
               age == "Age-standardized" &
               metric == "Rate (per 100,000)" &
               year %in% c(2020, 2021)) %>%
  dplyr::select(year, val)

actual_deaths_data <- EC2 %>%
  dplyr::filter(counterfactual == "Associated" &
               measure == "Deaths" & 
               pathogen == PATHOGEN_NAME &
               age == "All Ages" &
               metric == "Number" &
               year %in% c(2020, 2021)) %>%
  dplyr::select(year, val)

export_table$Actual_ASR <- actual_asr_data$val
export_table$Actual_Deaths <- actual_deaths_data$val

# Add predicted data
predicted_asr_2020_2021 <- Male_ASR %>%
  dplyr::filter(year %in% c(2020, 2021)) %>%
  dplyr::select(year, mean)

predicted_deaths_2020_2021 <- Male_sum_year %>%
  dplyr::filter(year %in% c(2020, 2021)) %>%
  dplyr::select(year, number)

export_table$Predicted_ASR <- predicted_asr_2020_2021$mean
export_table$Predicted_Deaths <- predicted_deaths_2020_2021$number

# Compute prediction errors
export_table$ASR_Error <- abs(export_table$Predicted_ASR - export_table$Actual_ASR)
export_table$Deaths_Error <- abs(export_table$Predicted_Deaths - export_table$Actual_Deaths)
export_table$ASR_Relative_Error <- (export_table$ASR_Error / export_table$Actual_ASR) * 100
export_table$Deaths_Relative_Error <- (export_table$Deaths_Error / export_table$Actual_Deaths) * 100

print("========================================")
print("Final summary of predictions")
print("========================================")
print(export_table)

# =============================================================================
# 7. Statistical analysis: COVID-19 impact on AMR mortality burden
# =============================================================================

cat("\n========================================\n")
cat("Statistical analysis of AMR mortality changes during COVID-19\n") 
cat("========================================\n")

# Extract predictions for 2020–2021 (use posterior percentiles; report 95% CrI)
covid_years <- c(2020, 2021)
predicted_asr_ci <- Male_ASR %>%
  dplyr::filter(year %in% covid_years) %>%
  dplyr::select(year, mean, `0.025Q`, `0.975Q`) %>%
  rename(predicted_asr = mean, 
         predicted_asr_lower = `0.025Q`,
         predicted_asr_upper = `0.975Q`)

predicted_deaths_ci <- Male_sum_year %>%
  dplyr::filter(year %in% covid_years) %>%
  dplyr::select(year, number, number_lower, number_upper) %>%
  rename(predicted_deaths = number,
         predicted_deaths_lower = number_lower,
         predicted_deaths_upper = number_upper)

# Extract observed data
actual_asr_ci <- EC2 %>%
      dplyr::filter(counterfactual == "Associated" &
                  measure == "Deaths" & 
               pathogen == PATHOGEN_NAME &
                  age == "Age-standardized" &
                  metric == "Rate (per 100,000)" &
               year %in% covid_years) %>%
  dplyr::select(year, val) %>%
  rename(observed_asr = val)
    
actual_deaths_ci <- EC2 %>%
      dplyr::filter(counterfactual == "Associated" &
                  measure == "Deaths" & 
               pathogen == PATHOGEN_NAME &
                  age == "All Ages" &
                  metric == "Number" &
               year %in% covid_years) %>%
  dplyr::select(year, val) %>%
  rename(observed_deaths = val)

# Merge all pieces
covid_impact_analysis <- predicted_asr_ci %>%
  left_join(predicted_deaths_ci, by = "year") %>%
  left_join(actual_asr_ci, by = "year") %>%
  left_join(actual_deaths_ci, by = "year")

cat("Overview for COVID-19 years:\n")
print(covid_impact_analysis)

# Utility: compute 95% CrI for differences using posterior percentiles (observed as fixed)
calculate_rate_differences_cri <- function(observed, predicted_mean, predicted_lower, predicted_upper) {
  ard <- observed - predicted_mean
  ard_lower <- observed - predicted_upper
  ard_upper <- observed - predicted_lower
  rrd <- (observed - predicted_mean) / predicted_mean * 100
  rrd_lower <- (observed - predicted_upper) / predicted_mean * 100
  rrd_upper <- (observed - predicted_lower) / predicted_mean * 100
  ard_significant <- ard_lower > 0 | ard_upper < 0
  rrd_significant <- ard_significant
  return(list(
    ard = ard,
    ard_lower = ard_lower,
    ard_upper = ard_upper,
    ard_significant = ard_significant,
    rrd = rrd,
    rrd_lower = rrd_lower,
    rrd_upper = rrd_upper,
    rrd_significant = rrd_significant
  ))
}

# ASR differences (95% CrI)
cat("\n=== Age-standardised mortality rate (ASR) analysis (95% CrI) ===\n")

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
    ard_asr_significant = diff_result$ard_significant,
    rrd_asr = diff_result$rrd,
    rrd_asr_lower = diff_result$rrd_lower,
    rrd_asr_upper = diff_result$rrd_upper,
    rrd_asr_significant = diff_result$rrd_significant
  )
  
  asr_differences <- rbind(asr_differences, year_result)
}

# Average differences for 2020–2021 (point estimates: yearly means)
mean_ard_asr <- mean(asr_differences$ard_asr, na.rm = TRUE)
mean_rrd_asr <- mean(asr_differences$rrd_asr, na.rm = TRUE)

# Bayesian summary: derive joint CrI via posterior draws for both years
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

# ASR posterior draws (assume independent log-normal per year)
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

# Statistical significance based on joint CrI
mean_ard_asr_significant <- combined_ard_asr_lower > 0 | combined_ard_asr_upper < 0
mean_rrd_asr_significant <- combined_rrd_asr_lower > 0 | combined_rrd_asr_upper < 0

cat("Year-wise ASR differences:\n")
print(asr_differences)

cat("\nAverage ASR difference (2020–2021):\n")
cat("Absolute rate difference (ARD):", sprintf("%.4f (95%% CrI: %.4f to %.4f)", mean_ard_asr, combined_ard_asr_lower, combined_ard_asr_upper), "\n")
cat("Relative rate difference (RRD):", sprintf("%.2f%% (95%% CrI: %.2f%% to %.2f%%)", mean_rrd_asr, combined_rrd_asr_lower, combined_rrd_asr_upper), "\n")
cat("ARD significance:", ifelse(combined_ard_asr_lower > 0 | combined_ard_asr_upper < 0, "Significant", "Not significant"), "\n")
cat("RRD significance:", ifelse(combined_rrd_asr_lower > 0 | combined_rrd_asr_upper < 0, "Significant", "Not significant"), "\n")

# Death counts difference (approx 95% CrI via totals)
cat("\n=== Death counts analysis (approx 95% CrI) ===\n")

deaths_differences <- data.frame()
for(i in 1:nrow(covid_impact_analysis)) {
  year <- covid_impact_analysis$year[i]
  observed <- covid_impact_analysis$observed_deaths[i]
  predicted <- covid_impact_analysis$predicted_deaths[i]
  predicted_lower <- covid_impact_analysis$predicted_deaths_lower[i]
  predicted_upper <- covid_impact_analysis$predicted_deaths_upper[i]
  
  diff_result <- calculate_rate_differences_cri(observed, predicted, predicted_lower, predicted_upper)
  
  year_result <- data.frame(
    year = year,
    observed_deaths = observed,
    predicted_deaths = predicted,
    ard_deaths = diff_result$ard,
    ard_deaths_lower = diff_result$ard_lower,
    ard_deaths_upper = diff_result$ard_upper,
    ard_deaths_significant = diff_result$ard_significant,
    rrd_deaths = diff_result$rrd,
    rrd_deaths_lower = diff_result$rrd_lower,
    rrd_deaths_upper = diff_result$rrd_upper,
    rrd_deaths_significant = diff_result$rrd_significant
  )
  
  deaths_differences <- rbind(deaths_differences, year_result)
}

# Average difference for deaths in 2020–2021; CrI via posterior draws
mean_ard_deaths <- mean(deaths_differences$ard_deaths, na.rm = TRUE)
mean_rrd_deaths <- mean(deaths_differences$rrd_deaths, na.rm = TRUE)

# 死亡数后验抽样（对数正态近似）
deaths_ci_ord <- predicted_deaths_ci %>% dplyr::arrange(year)
deaths_obs_ord <- actual_deaths_ci %>% dplyr::arrange(year)

params_deaths_2020 <- infer_lognorm_params(deaths_ci_ord$predicted_deaths_lower[1], deaths_ci_ord$predicted_deaths_upper[1])
params_deaths_2021 <- infer_lognorm_params(deaths_ci_ord$predicted_deaths_lower[2], deaths_ci_ord$predicted_deaths_upper[2])

pred_deaths_2020_draws <- rlnorm(n_draws, params_deaths_2020$meanlog, params_deaths_2020$sdlog)
pred_deaths_2021_draws <- rlnorm(n_draws, params_deaths_2021$meanlog, params_deaths_2021$sdlog)

ard_deaths_2020_draws <- deaths_obs_ord$observed_deaths[1] - pred_deaths_2020_draws
ard_deaths_2021_draws <- deaths_obs_ord$observed_deaths[2] - pred_deaths_2021_draws

rrd_deaths_2020_draws <- (deaths_obs_ord$observed_deaths[1] - pred_deaths_2020_draws) / pred_deaths_2020_draws * 100
rrd_deaths_2021_draws <- (deaths_obs_ord$observed_deaths[2] - pred_deaths_2021_draws) / pred_deaths_2021_draws * 100

combined_ard_deaths_draws <- (ard_deaths_2020_draws + ard_deaths_2021_draws) / 2
combined_rrd_deaths_draws <- (rrd_deaths_2020_draws + rrd_deaths_2021_draws) / 2

combined_ard_deaths_lower <- as.numeric(quantile(combined_ard_deaths_draws, 0.025, na.rm = TRUE))
combined_ard_deaths_upper <- as.numeric(quantile(combined_ard_deaths_draws, 0.975, na.rm = TRUE))
combined_rrd_deaths_lower <- as.numeric(quantile(combined_rrd_deaths_draws, 0.025, na.rm = TRUE))
combined_rrd_deaths_upper <- as.numeric(quantile(combined_rrd_deaths_draws, 0.975, na.rm = TRUE))

# Statistical significance after quantiles
mean_ard_deaths_significant <- combined_ard_deaths_lower > 0 | combined_ard_deaths_upper < 0
mean_rrd_deaths_significant <- combined_rrd_deaths_lower > 0 | combined_rrd_deaths_upper < 0

cat("Year-wise deaths difference:\n")
print(deaths_differences)

cat("\nAverage deaths difference (2020–2021):\n")
cat("Absolute difference (ARD):", sprintf("%.0f (95%% CrI: %.0f to %.0f)", mean_ard_deaths, combined_ard_deaths_lower, combined_ard_deaths_upper), "\n")
cat("Relative difference (RRD):", sprintf("%.2f%% (95%% CrI: %.2f%% to %.2f%%)", mean_rrd_deaths, combined_rrd_deaths_lower, combined_rrd_deaths_upper), "\n")
cat("ARD significance:", ifelse(combined_ard_deaths_lower > 0 | combined_ard_deaths_upper < 0, "Significant", "Not significant"), "\n")
cat("RRD significance:", ifelse(combined_rrd_deaths_lower > 0 | combined_rrd_deaths_upper < 0, "Significant", "Not significant"), "\n")

# Build a combined summary table
covid_impact_summary <- data.frame(
  Metric = c("Age-standardised mortality rate (per 100k)", "Deaths"),
  Mean_ARD = c(mean_ard_asr, mean_ard_deaths),
  ARD_95CrI_Lower = c(combined_ard_asr_lower, combined_ard_deaths_lower),
  ARD_95CrI_Upper = c(combined_ard_asr_upper, combined_ard_deaths_upper),
  ARD_Significant = c(combined_ard_asr_lower > 0 | combined_ard_asr_upper < 0,
                      combined_ard_deaths_lower > 0 | combined_ard_deaths_upper < 0),
  Mean_RRD_Percent = c(mean_rrd_asr, mean_rrd_deaths),
  RRD_95CrI_Lower = c(combined_rrd_asr_lower, combined_rrd_deaths_lower),
  RRD_95CrI_Upper = c(combined_rrd_asr_upper, combined_rrd_deaths_upper),
  RRD_Significant = c(combined_rrd_asr_lower > 0 | combined_rrd_asr_upper < 0,
                      combined_rrd_deaths_lower > 0 | combined_rrd_deaths_upper < 0)
)

cat("\n========================================\n")
cat("Comprehensive analysis: COVID-19 impact on ", PATHOGEN_NAME, " mortality burden\n")
cat("========================================\n")
print(covid_impact_summary)

# Final counterfactual summary (Number and Rate with 95% CrI)
final_counterfactual_table <- data.frame(
  Counterfactual = rep("Associated", nrow(covid_impact_analysis)),
  Pathogens = rep(PATHOGEN_NAME, nrow(covid_impact_analysis)),
  Periods = covid_impact_analysis$year,
  # Number 部分
  `Number Observed` = covid_impact_analysis$observed_deaths,
  `Number Predicted` = covid_impact_analysis$predicted_deaths,
  `Number Absolute difference` = deaths_differences$ard_deaths,
  `Number Absolute difference 95% CrI Lower` = deaths_differences$ard_deaths_lower,
  `Number Absolute difference 95% CrI Upper` = deaths_differences$ard_deaths_upper,
  `Number Relative change (%)` = deaths_differences$rrd_deaths,
  `Number Relative change (%) 95% CrI Lower` = deaths_differences$rrd_deaths_lower,
  `Number Relative change (%) 95% CrI Upper` = deaths_differences$rrd_deaths_upper,
  # Rate 部分（per 100k）
  `Rate (per 100k) Observed` = covid_impact_analysis$observed_asr,
  `Rate (per 100k) Predicted` = covid_impact_analysis$predicted_asr,
  `Rate (per 100k) Absolute difference` = asr_differences$ard_asr,
  `Rate (per 100k) Absolute difference 95% CrI Lower` = asr_differences$ard_asr_lower,
  `Rate (per 100k) Absolute difference 95% CrI Upper` = asr_differences$ard_asr_upper,
  `Rate (per 100k) Relative change (%)` = asr_differences$rrd_asr,
  `Rate (per 100k) Relative change (%) 95% CrI Lower` = asr_differences$rrd_asr_lower,
  `Rate (per 100k) Relative change (%) 95% CrI Upper` = asr_differences$rrd_asr_upper,
  stringsAsFactors = FALSE
)

# Save detailed outputs
covid_analysis_filename <- paste0(gsub(" ", "_", PATHOGEN_NAME), "_covid_impact_analysis.csv")
# 输出按两位小数与合并95%CI的精简表（Observed / Predicted / Absolute difference (95%CI) / Relative change (95%CI)）
asr_differences_formatted <- asr_differences %>%
  dplyr::transmute(
    Observed = sprintf("%.2f", observed_asr),
    Predicted = sprintf("%.2f", predicted_asr),
    `Absolute difference (95%CI)` = sprintf("%.2f (%.2f, %.2f)", ard_asr, ard_asr_lower, ard_asr_upper),
    `Relative change (95%CI)` = sprintf("%.2f%% (%.2f%%, %.2f%%)", rrd_asr, rrd_asr_lower, rrd_asr_upper)
  )

write.csv(asr_differences_formatted, paste0(gsub(" ", "_", PATHOGEN_NAME), "_asr_differences.csv"), row.names = FALSE)

  # Add a version with period summary (2020, 2021, and 2020–2021)
  # Include holdout (2017–2019) RMSE/MAE
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

# Add a 2020–2021 summary row (use yearly means)
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

write.csv(asr_with_period, paste0(gsub(" ", "_", PATHOGEN_NAME), "_asr_differences_with_2020_2021.csv"), row.names = FALSE)
write.csv(deaths_differences, paste0(gsub(" ", "_", PATHOGEN_NAME), "_deaths_differences.csv"), row.names = FALSE)
write.csv(covid_impact_summary, covid_analysis_filename, row.names = FALSE)
write.csv(final_counterfactual_table, paste0(gsub(" ", "_", PATHOGEN_NAME), "_counterfactual_full_table.csv"), row.names = FALSE)

cat("\nSaved outputs:\n")
cat("- ASR differences:", paste0(gsub(" ", "_", PATHOGEN_NAME), "_asr_differences.csv"), "\n")
cat("- Deaths differences:", paste0(gsub(" ", "_", PATHOGEN_NAME), "_deaths_differences.csv"), "\n")
cat("- COVID-19 impact summary:", covid_analysis_filename, "\n")
cat("- Counterfactual full table:", paste0(gsub(" ", "_", PATHOGEN_NAME), "_counterfactual_full_table.csv"), "\n")

# Save main prediction table
output_filename <- paste0(gsub(" ", "_", PATHOGEN_NAME), "_optimized_bapc_results.csv")
write.csv(export_table, output_filename, row.names = FALSE)

# Save optimization parameters (if any)
param_filename <- paste0(gsub(" ", "_", PATHOGEN_NAME), "_optimal_parameters.csv")
if(nrow(selection_result$all_results) > 0) {
  write.csv(selection_result$all_results, param_filename, row.names = FALSE)
  cat("Saved parameter optimization results\n")
    } else {
  cat("Used default parameters; no optimization results saved\n")
}

cat("\n========================================\n")
cat("分析完成！\n")
cat("病原体:", PATHOGEN_NAME, "\n")
cat("最优参数 - gf:", optimal_params$gf, "secondDiff:", optimal_params$secondDiff, "\n")
if(!is.na(optimal_params$rmse)) {
  cat("验证性能 - RMSE:", round(optimal_params$rmse, 4), "MAE:", round(optimal_params$mae, 4), "\n")
    } else {
  cat("使用默认参数（优化失败）\n")
}
cat("结果文件:", output_filename, "\n")
cat("参数文件:", param_filename, "\n")
cat("========================================\n")

# =============================================================================
# 8. Interpretation and clinical implications
# =============================================================================

cat("\n========================================\n")
cat("Interpretation of findings\n")
cat("========================================\n")

cat("We used a BAPC model to evaluate the COVID-19 impact on mortality related to ", PATHOGEN_NAME, ":\n\n")

cat("1. Model performance:\n")
cat("   - Trained on 1990–2016; validated on 2017–2019\n")
cat("   - Optimal params: gf =", optimal_params$gf, ", secondDiff =", optimal_params$secondDiff, "\n")
if(!is.na(optimal_params$rmse)) {
  cat("   - Validation performance: RMSE =", round(optimal_params$rmse, 4), ", MAE =", round(optimal_params$mae, 4), "\n")
}

cat("\n2. Impact during COVID-19:\n")
if(exists("mean_ard_asr_significant") && exists("mean_rrd_asr_significant")) {
  cat("   Age-standardised mortality:\n")
  cat("   - Absolute difference:", sprintf("%.4f per 100k", mean_ard_asr), 
      ifelse(mean_ard_asr > 0, " (increase)", " (decrease)"), "\n")
  cat("   - Relative difference:", sprintf("%.2f%%", mean_rrd_asr), 
      ifelse(mean_rrd_asr > 0, " (increase)", " (decrease)"), "\n")
  cat("   - Significance:", ifelse(mean_ard_asr_significant, "Significant", "Not significant"), "\n")
  
  cat("   Death counts:\n")
  cat("   - Absolute difference:", sprintf("%.0f", mean_ard_deaths), 
      ifelse(mean_ard_deaths > 0, " (increase)", " (decrease)"), "\n")
  cat("   - Relative difference:", sprintf("%.2f%%", mean_rrd_deaths), 
      ifelse(mean_rrd_deaths > 0, " (increase)", " (decrease)"), "\n")
  cat("   - Significance:", ifelse(mean_ard_deaths_significant, "Significant", "Not significant"), "\n")
}

cat("\n3. Clinical and public health implications:\n")
if(exists("mean_rrd_asr") && mean_rrd_asr > 0) {
  cat("   - The mortality burden related to ", PATHOGEN_NAME, " increased significantly during COVID-19\n")
  cat("   - Highlights need for stronger AMR surveillance and infection control\n")
  cat("   - Provides evidence for targeted public health policy\n")
} else if(exists("mean_rrd_asr") && mean_rrd_asr < 0) {
  cat("   - The mortality burden related to ", PATHOGEN_NAME, " decreased during COVID-19\n")
  cat("   - May relate to resource reallocation or stronger infection control\n")
  cat("   - Further investigation is warranted\n")
    } else {
  cat("   - The mortality burden related to ", PATHOGEN_NAME, " showed no marked change during COVID-19\n")
  cat("   - Suggests current measures may have maintained burden levels\n")
}

cat("\n4. Limitations:\n")
  cat("   - Forecast based on historical trends; does not model direct COVID-19 effects\n")
  cat("   - Accuracy of death statistics depends on input data quality\n")
  cat("   - CrI reflect uncertainty in the model predictions\n")

# =============================================================================
# 9. 最终统计分析结果表格输出
# =============================================================================

cat("\n\n")
cat("########################################\n")
cat("Statistical analysis of AMR mortality changes during COVID-19\n")
cat("########################################\n")

if(exists("mean_ard_asr_significant") && exists("mean_rrd_asr_significant")) {
  # Build a compact summary table
  summary_table <- data.frame(
    指标 = c("Age-standardised mortality (ASR)", "Deaths"),
    绝对率差异_ARD = c(
      sprintf("%.4f", mean_ard_asr),
      sprintf("%.0f", mean_ard_deaths)
    ),
    ARD_95CI = c(
      sprintf("(%.4f, %.4f)", combined_ard_asr_lower, combined_ard_asr_upper),
      sprintf("(%.0f, %.0f)", combined_ard_deaths_lower, combined_ard_deaths_upper)
    ),
    相对率差异_RRD = c(
      sprintf("%.2f%%", mean_rrd_asr),
      sprintf("%.2f%%", mean_rrd_deaths)
    ),
    RRD_95CI = c(
      sprintf("(%.2f%%, %.2f%%)", combined_rrd_asr_lower, combined_rrd_asr_upper),
      sprintf("(%.2f%%, %.2f%%)", combined_rrd_deaths_lower, combined_rrd_deaths_upper)
    ),
    统计显著性 = c(
      ifelse(mean_ard_asr_significant, "显著", "不显著"),
      ifelse(mean_ard_deaths_significant, "显著", "不显著")
    ),
    stringsAsFactors = FALSE
  )
  
  # Print table header
  cat("\n")
  cat(sprintf("%-20s %-12s %-22s %-12s %-22s %-8s\n", 
              "指标", "绝对差异(ARD)", "ARD 95% CI", "相对差异(RRD)", "RRD 95% CI", "显著性"))
  cat(paste(rep("=", 106), collapse = ""), "\n")
  
  # Print table rows
  for(i in 1:nrow(summary_table)) {
    cat(sprintf("%-20s %-12s %-22s %-12s %-22s %-8s\n",
                summary_table$指标[i],
                summary_table$绝对率差异_ARD[i],
                summary_table$ARD_95CI[i],
                summary_table$相对率差异_RRD[i],
                summary_table$RRD_95CI[i],
                summary_table$统计显著性[i]))
  }
  
  cat(paste(rep("=", 106), collapse = ""), "\n")
  
  # Add brief notes
  cat("\nNotes:\n")
  cat("- ARD (absolute rate difference) = observed - expected\n")
  cat("- RRD (relative rate difference) = (observed - expected) / expected × 100%\n")
  cat("- Significance: 95% CI lower > 0 indicates significant increase\n")
  cat("- Pathogen:", PATHOGEN_NAME, "\n")
  cat("- Period: 2020–2021 vs expected (based on 1990–2019 trend)\n")
  
  # Save summary to CSV
  table_filename <- paste0(gsub(" ", "_", PATHOGEN_NAME), "_statistical_analysis_table.csv")
  write.csv(summary_table, table_filename, row.names = FALSE, fileEncoding = "UTF-8")
  cat("- 统计分析表格已保存:", table_filename, "\n")
  
} else {
  cat("Statistical analysis unavailable; please check data processing.\n")
}

cat("\n========================================\n")
cat("Completed at:", Sys.time(), "\n")
cat("========================================\n")
