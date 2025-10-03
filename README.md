# Antimicrobial Resistance Mortality Analysis - Code Repository

This repository contains the complete code and data for reproducing the analyses presented in our Nature Communications submission on global antimicrobial resistance mortality trends.

## Repository Structure

```
├── Figure1/          # Temporal trends in AMR mortality (1990-2021)
│   ├── Figure1A.R    # Global mortality trends
│   ├── Figure1B.R    # Mortality rate trends
│   ├── Figure1C.R    # Regional trends
│   ├── Figure1D.R    # Age-standardized rates
│   └── data/         # Source data
│
├── Figure2/          # Geographic distribution of AMR mortality
│   ├── Figure2A.R    # Global map visualization
│   ├── Figure2B.R    # Regional distribution
│   ├── Figure2C.R    # Country-level analysis
│   ├── Figure2D.R    # Temporal changes
│   ├── utils.R       # Helper functions
│   └── data/         # Geographic and mortality data
│
├── Figure3/          # Age-Period-Cohort analysis - Regional trends
│   ├── Figure3A.R    # Global and regional APC trends
│   ├── Figure3B.R    # Proportional mortality analysis
│   ├── Figure3C.R    # Trend visualization
│   └── data/         # APC model outputs by region
│
├── Figure4/          # Age, Period, and Cohort effects decomposition
│   ├── age effect.R      # Age-specific mortality patterns
│   ├── period effect.R   # Period effects analysis
│   ├── Cohort effect.R   # Birth cohort effects
│   └── data/             # Effect decomposition data
│
├── Figure5/          # Pathogen-specific mortality analysis
│   ├── Fig_middle1.R     # Middle panel analysis 1
│   ├── Fig_middle2.R     # Middle panel analysis 2
│   ├── Fig_middle3.R     # Middle panel analysis 3
│   ├── Fig_right1.R      # Right panel analysis
│   ├── Fig6_left.R       # Left panel analysis
│   └── data/             # Pathogen-specific data
│
└── Figure6/          # Future mortality projections
    ├── Figure6.R     # Projection models and visualization
    └── data/         # Population and parameter data
```

## Requirements

### Software
- R (version ≥ 4.0.0)
- RStudio (recommended)

### R Packages
Main packages used across analyses:
```r
# Data manipulation
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)

# Visualization
library(ggplot2)
library(patchwork)
library(scales)

# Spatial analysis
library(sf)
library(maps)

# Statistical modeling
library(mgcv)
# (Additional packages are loaded in individual scripts)
```

## Installation

1. Clone this repository:
```bash
git clone https://github.com/zhihuic40-cell/AMRGBD-code-availability.git
cd AMRGBD-code-availability
```

2. Install required R packages:
```r
# Install packages if needed
install.packages(c("tidyverse", "ggplot2", "patchwork", "scales", "sf", "maps", "mgcv"))
```

## Usage

Each figure can be reproduced independently by running the corresponding R scripts:

```r
# Example: Generate Figure 1A
source("Figure1/Figure1A.R")
```

**Note:** 
- Ensure the working directory is set to the repository root
- Data files are automatically loaded from the respective `data/` folders
- Scripts use relative paths for data loading

## Data Description

### Figure 1 Data
- Mortality counts and rates from 1990-2021
- Age-standardized mortality rates
- Regional and global aggregates

### Figure 2 Data
- Country-level mortality data
- Geographic boundary files (shapefiles)
- Location metadata

### Figure 3-5 Data
- Age-Period-Cohort model outputs (.rds files)
- Proportional mortality estimates
- Pathogen-specific mortality data

### Figure 6 Data
- GBD population projections (2017-2100)
- Age-standardized rate parameters
- Model fitting results

## Key Analyses

1. **Temporal Trend Analysis**: Long-term trends in AMR mortality from 1990 to 2021
2. **Geographic Distribution**: Spatial patterns and regional disparities
3. **Age-Period-Cohort Decomposition**: Disentangling age, period, and cohort effects
4. **Pathogen-Specific Analysis**: Mortality attributed to different bacterial pathogens
5. **Future Projections**: Forecasting AMR mortality to 2050

## Citation

If you use this code or data, please cite:

```
[Citation information will be added upon publication]
```

## License

This code is available for academic and research purposes. For commercial use, please contact the authors.

## Contact

For questions or issues regarding the code, please:
- Open an issue in this repository
- Contact: [Your contact email]

## Acknowledgments

This work uses data from the Global Burden of Disease Study 2021.

## Updates

- **2025-10**: Initial repository setup for Nature Communications submission

