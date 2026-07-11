# Global Spatiotemporal Dynamics of Mortality from Carbapenem-Resistant Gram-Negative Infections

Code and data for reproducing all analyses in:

> Chen Z, Zhong M, Wu J, et al. Global spatiotemporal dynamics of mortality from carbapenem-resistant Gram-negative infections over the past three decades. *Nature Communications* (under review).

## Quick Start

```bash
git clone https://github.com/zhihuic40-cell/AMRGBD-code-availability.git
cd AMRGBD-code-availability
```

**Option A — RStudio (recommended):** Open `AMRGBD.Rproj`, then:

```r
source("code/install.R")   # first time only
source("run_all.R")
```

**Option B — Command line:**

```bash
cd code
Rscript install.R
Rscript run_all.R
```

Estimated runtime: 2–3 hours (Figures 1–5, S1 finish in minutes; Figure 6 is computationally intensive).

## Repository Structure

```
├── run_all.R                  # Entry point
├── AMRGBD.Rproj               # RStudio project
│
├── code/
│   ├── install.R              # Package installation
│   ├── run_all.R              # Master driver
│   ├── Figure1/               # Pathogen contributions & regional proportions
│   ├── Figure2/               # Global maps (ASR & net drift, 2021)
│   ├── Figure3/               # Age-specific mortality trends
│   ├── Figure4/               # Age-Period-Cohort decomposition (regional)
│   ├── Figure5/               # Pathogen-specific APC (associated)
│   ├── FigureS1/              # Pathogen-specific APC (attributable)
│   ├── Figure6/               # COVID-19 pandemic impact (BAPC-INLA)
│   └── Sensitivity/           # Monte Carlo sensitivity analyses
│       ├── TableS8/           #   APC net drift with input uncertainty
│       └── TableS9/           #   BAPC counterfactual with input uncertainty
│
├── data/                      # Input data organized by figure
│   ├── Figure1–6, FigureS1/   # CSVs, RDS, XLSX, shapefiles
│   └── sensitivity_easyGBDR/  # Sensitivity analysis inputs + pre-generated draws
│
└── results/                   # Generated outputs (PDFs, CSVs)
```

## Analyses and Outputs

| Figure/Table | Scripts | Description | Key output |
|---|---|---|---|
| Figure 1 | 4 | Pathogen contributions to CRGN deaths (2021) | 4 PDFs |
| Figure 2 | 5 | Country-level ASR maps & net drift maps | 4 PDFs |
| Figure 3 | 3 | Age-specific mortality trends & age distributions | 3 PDFs |
| Figure 4 | 3 | Regional age, period, and cohort effects | 3 PDFs |
| Figure 5 | 5 | Pathogen-specific APC (associated mortality) | 28 PDFs |
| Figure S1 | 5 | Pathogen-specific APC (attributable mortality) | 28 PDFs |
| Figure 6 | 16 | Bayesian counterfactual: COVID-19 impact | 16 PDFs + 32 CSVs |
| Table S8 | 6 | Sensitivity: APC net drift (500 MC draws) | Summary CSVs |
| Table S9 | 16 | Sensitivity: BAPC counterfactual (500 MC draws) | Summary CSVs |

**Note:** `run_all.R` executes Figures 1–5, S1, and one Figure 6 script by default. To run all 16 Figure 6 scripts, uncomment the relevant lines in `code/run_all.R`.

## Requirements

- **R** >= 4.0.0 (RStudio recommended)
- Run `code/install.R` to install all packages, or manually:

```r
# Core
install.packages(c("tidyverse", "readxl", "data.table", "here"))

# Visualization
install.packages(c("ggplot2", "patchwork", "cowplot", "ggpubr",
                   "scales", "RColorBrewer", "ggsci", "viridis"))

# Spatial (Figure 2; Linux/Mac may need libgdal-dev, libgeos-dev, libproj-dev)
install.packages(c("sf", "rnaturalearth", "rnaturalearthdata"))

# Bayesian modeling (Figure 6 & Table S9)
install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)
install.packages("BAPC", repos = "http://R-Forge.R-project.org")
```

### Sensitivity analysis dependencies

- **Table S8** additionally requires `easyGBDR`, a validated R implementation of the [NCI APC Web Tool](https://analysistools.cancer.gov/apc/) (commercial licence required). Individual results can be verified using the freely available NCI Web Tool.
- **Table S9** uses only open-source packages (INLA, BAPC) and requires that the corresponding Figure 6 analysis has been run first (to obtain optimal model parameters).

## Data Source

All mortality estimates are from the [Global Research on Antimicrobial Resistance (GRAM) project](https://vizhub.healthdata.org/microbe/), covering 204 countries and territories, 1990–2021.

> Global burden of bacterial antimicrobial resistance 1990–2021: a systematic analysis with forecasts to 2050. *Lancet* 2024; 404: 1199–1226.

## Citation

```
Chen Z, Zhong M, Wu J, et al. Global spatiotemporal dynamics of mortality from
carbapenem-resistant Gram-negative infections over the past three decades.
Nature Communications (under review), 2025.
```

## License

This code is provided for academic and non-commercial research purposes. For other use, please contact the corresponding authors.

## Contact

- **Wenhong Zhang, MD, PhD** — zhangwenhong@fudan.edu.cn
- **Jialin Jin, MD, PhD** — jinjialin@fudan.edu.cn

Department of Infectious Diseases, Huashan Hospital, Fudan University

For technical questions, please open an issue or contact the first authors.
