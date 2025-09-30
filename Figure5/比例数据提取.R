# Install and load required packages
if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(readxl)) {
  install.packages("readxl")
}

library(dplyr)
library(readxl)

# Read Excel file
# Use file.path() to build a portable path
file_path <- file.path("data", "ASSproportion数据提取.xls")
data <- read_excel(file_path)

# Compute the proportion of each age group within each pathogen-year
result <- data %>%
  # Group by pathogen and year
  group_by(Pathogen, year) %>%
  # Compute within-group proportions
  mutate(proportion = val / sum(val) * 100) %>%
  # Select required columns
  select(Pathogen, age_name, year, val, proportion) %>%
  # Arrange by pathogen, year, and descending proportion
  arrange(Pathogen, year, desc(proportion))

# Preview a few rows of the result
print("Preview of first rows:")
print(head(result))

# Show summary statistics per pathogen
summary_stats <- result %>%
  group_by(Pathogen) %>%
  summarise(
    total_val = sum(val),
    mean_proportion = mean(proportion),
    max_proportion = max(proportion),
    min_proportion = min(proportion)
  )

print("\nSummary by pathogen:")
print(summary_stats)

# Save results to an Excel file
if (!require(writexl)) {
  install.packages("writexl")
}
library(writexl)

# Write results to an Excel file
write_xlsx(result, "ASSproportion.xlsx")

# Output validation info
cat("\nData processing completed!")
cat("\nAge-group counts per pathogen:\n")
table(result$Pathogen)

# Verify proportions sum to ~100 for each pathogen-year combination
proportion_check <- result %>%
  group_by(Pathogen, year) %>%
  summarise(total_proportion = sum(proportion)) %>%
  ungroup()

cat("\nProportion check (should be close to 100):\n")
print(head(proportion_check))