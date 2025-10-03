# Global Spatiotemporal Dynamics of Mortality from Carbapenem-Resistant Gram-Negative Infections

This repository contains the complete code and data for reproducing the analyses presented in our Nature Communications submission:

**"Global spatiotemporal dynamics of mortality from carbapenem-resistant Gram-negative infections over the past three decades"**

Zhihui Chen, Mingchen Zhong, Jing Wu, et al.

## Repository Structure

```
├── Figure1/          # Pathogen contributions and regional proportions (2021)
│   ├── Figure1A.R    # Pathogen percentage contributions to CRGN deaths
│   ├── Figure1B.R    # Proportion within total AMR deaths (associated)
│   ├── Figure1C.R    # Pathogen contributions to attributable deaths
│   ├── Figure1D.R    # Proportion within total AMR deaths (attributable)
│   └── data/         # Death counts and proportions (1990-2021)
│
├── Figure2/          # Global geographical distribution and temporal trends
│   ├── Figure2A.R    # Associated mortality rates map (2021)
│   ├── Figure2B.R    # Attributable mortality rates map (2021)
│   ├── Figure2C.R    # Net drift for associated deaths (1990-2019)
│   ├── Figure2D.R    # Net drift for attributable deaths (1990-2019)
│   ├── utils.R       # Mapping helper functions
│   └── data/         # Shapefiles and country-level mortality data
│
├── Figure3/          # Age-specific trends and death distribution
│   ├── Figure3A.R    # Local drift by age group (1990-2019)
│   ├── Figure3B.R    # Age distribution of associated deaths (1990-2021)
│   ├── Figure3C.R    # Age distribution of attributable deaths (1990-2021)
│   └── data/         # Regional APC outputs and proportions
│
├── Figure4/          # Age-Period-Cohort decomposition
│   ├── age effect.R      # Longitudinal age curves
│   ├── period effect.R   # Period rate ratios (1990-2019)
│   ├── Cohort effect.R   # Birth cohort rate ratios (1895-2015)
│   └── data/             # APC model outputs for 7 regions
│
├── Figure5/          # Pathogen-specific age-period-cohort patterns
│   ├── Fig6_left.R       # Age distribution by pathogen
│   ├── Fig_middle1.R     # Local drift by pathogen
│   ├── Fig_middle2.R     # Age effects by pathogen
│   ├── Fig_middle3.R     # Period effects by pathogen
│   ├── Fig_right1.R      # Cohort effects by pathogen
│   └── data/             # Pathogen-specific APC outputs
│
└── Figure6/          # COVID-19 pandemic impact (counterfactual analysis)
    ├── Figure6.R     # Bayesian predictions vs observed (2020-2021)
    └── data/         # Pre-pandemic data and projection parameters
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
library(RColorBrewer)

# Spatial analysis
library(sf)
library(rnaturalearth)

# Age-Period-Cohort modeling
# Uses NCI APC Web Tool: https://analysistools.cancer.gov/apc/

# Bayesian modeling (Figure 6)
library(INLA)

# Additional packages loaded in individual scripts
```

## Installation

1. Clone this repository:
```bash
git clone https://github.com/zhihuic40-cell/AMRGBD-code-availability.git
cd AMRGBD-code-availability
```

2. Install required R packages:
```r
# Install CRAN packages
install.packages(c("tidyverse", "dplyr", "readr", "readxl",
                   "ggplot2", "patchwork", "scales", "RColorBrewer",
                   "sf", "rnaturalearth", "rnaturalearthdata"))

# For Bayesian analysis (Figure 6), install INLA
install.packages("INLA", repos = c(getOption("repos"), 
                 INLA = "https://inla.r-inla-download.org/R/stable"), 
                 dep = TRUE)
```

3. For Age-Period-Cohort analysis (Figures 3-5):
   - Visit the NCI APC Web Tool: https://analysistools.cancer.gov/apc/
   - Or use the R implementation following methods described in the manuscript

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

## Study Overview

### Objective
To analyze global, regional, and national spatiotemporal dynamics of mortality attributable to and associated with carbapenem-resistant Gram-negative (CRGN) bacterial infections from 1990 to 2021.

### Key Pathogens Studied
- *Acinetobacter baumannii*
- *Citrobacter* spp.
- *Enterobacter* spp.
- *Escherichia coli*
- *Klebsiella pneumoniae*
- *Pseudomonas aeruginosa*
- *Serratia* spp.

### Two Mortality Metrics
1. **Associated deaths**: Deaths assuming counterfactual replacement with no infection
2. **Attributable deaths**: Deaths assuming counterfactual replacement with drug-susceptible infection

### Analytical Methods
- **Age-Period-Cohort (APC) decomposition**: To disentangle independent temporal effects of age, period, and birth cohort
- **Bayesian counterfactual modeling (BAPC-INLA)**: To quantify COVID-19 pandemic impact on mortality trajectories

## Data Description

### Data Source
All mortality estimates are derived from the [Global Research on Antimicrobial Resistance (GRAM) project](https://vizhub.healthdata.org/microbe/), which provides AMR burden estimates for 204 countries and territories from 1990 to 2021.

### Figure-Specific Data
- **Figure 1**: Pathogen-specific death counts and proportions within total AMR burden
- **Figure 2**: Country-level age-standardized mortality rates and net drift estimates with shapefiles
- **Figure 3**: Age-group-specific mortality trends and death distributions across 7 GBD super-regions
- **Figure 4**: APC model outputs (age effects, period rate ratios, cohort rate ratios) for global and regional analyses
- **Figure 5**: Pathogen-specific APC decomposition results for 7 bacterial species
- **Figure 6**: Pre-pandemic mortality data (1990-2019), Bayesian projections, and observed data (2020-2021)

## Key Findings

### Global Burden (2021)
- **1.03 million** deaths associated with CRGN infections (ASR: 12.99 per 100,000)
- **0.22 million** deaths attributable to CRGN infections (ASR: 2.71 per 100,000)

### Temporal Trends (1990-2019)
- Global mortality rates increased annually by **0.82%** (associated) and **0.90%** (attributable)
- *Klebsiella pneumoniae*: Fastest increase (**3.73%** per year for associated deaths)
- *Pseudomonas aeruginosa*: Divergent trends (associated mortality ↓0.35%; attributable mortality ↑0.17%)

### Geographic Disparities
- **South Asia**: Highest burden (29.91 per 100,000) and fastest increase (+1.52% per year)
- **High-income regions**: Sustained declines (-1.60% per year)
- Marked country-level heterogeneity (from +4.54% in Lesotho to -6.68% in Rwanda)

### Age-Period-Cohort Effects
- **Age**: Highest mortality in older adults (>400 per 100,000 at age 95-99)
- **Period**: Increasing risk globally; divergent regional trends
- **Cohort**: Rising risk in younger birth cohorts, especially in South Asia

### COVID-19 Pandemic Impact (2020-2021)
- Associated mortality: **-4.71%** reduction (significant)
- Attributable mortality: -3.10% reduction (not significant)
- Suggests reduced exposure to healthcare settings rather than diminished resistance harm
- Largest decline in *Pseudomonas aeruginosa* (-7.31% associated; -10.47% attributable)

## Citation

If you use this code or data, please cite:

```
Chen Z, Zhong M, Wu J, et al. Global spatiotemporal dynamics of mortality from 
carbapenem-resistant Gram-negative infections over the past three decades. 
[Under review at Nature Communications, 2025]
```

### Data Source Citation
```
Global burden of bacterial antimicrobial resistance 1990-2021: a systematic 
analysis with forecasts to 2050. Lancet 2024; 404: 1199-1226.
https://vizhub.healthdata.org/microbe/
```

## License

This code is made available for academic and non-commercial research purposes. For any other use, please contact the corresponding authors.

## Contact

**Corresponding Authors:**

**Wenhong Zhang, MD, PhD**  
Department of Infectious Diseases, Huashan Hospital, Fudan University  
Email: zhangwenhong@fudan.edu.cn

**Jialin Jin, MD, PhD**  
Department of Infectious Diseases, Huashan Hospital, Fudan University  
Email: jinjialin@fudan.edu.cn

For technical questions about the code:
- Open an issue in this repository
- Contact first authors: Zhihui Chen, Mingchen Zhong, or Jing Wu

## Acknowledgments

This research was supported by:
- National Key Research and Development Program of China (Grant 2022YFC2009802)
- Wenzhou Basic Scientific Research Project (Grant Y2023527)
- Taicang First People's Hospital Internal Research Project (Grant 2025-IIT-074)

Data source: Institute for Health Metrics and Evaluation (IHME) Global Research on Antimicrobial Resistance (GRAM) project.

## Updates

- **2025-10**: Initial repository setup for Nature Communications submission
- Code corresponds to manuscript version 6.0

