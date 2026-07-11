# Figure 6 - COVID-19 Impact Analysis

## Overview

Figure 6 contains **16 independent R scripts** analyzing the impact of COVID-19 pandemic on carbapenem-resistant Gram-negative bacterial infections.

**Analysis Coverage:**
- **8 Pathogens** × **2 Counterfactuals** = **16 Scripts**

---

## Pathogens Analyzed

1. Acinetobacter baumannii
2. Citrobacter spp.
3. Enterobacter spp.
4. Escherichia coli
5. Klebsiella pneumoniae
6. Pseudomonas aeruginosa
7. Serratia spp.
8. GNB (All Gram-negative bacteria combined)

---

## Counterfactual Scenarios

1. **Associated** - Deaths associated with carbapenem resistance
2. **Attributable** - Deaths attributable to carbapenem resistance

---

## File Structure

### R Scripts (16 files)
```
Figure6_[Pathogen]_[Counterfactual].R
```

**Examples:**
- `Figure6_Escherichia_coli_Associated.R`
- `Figure6_Klebsiella_pneumoniae_Attributable.R`
- `Figure6_GNB_Associated.R`

### Data Files
- `data.csv` - Main mortality data
- `GBD2021 world population age standard.csv` - Age standardization weights
- `IHME_POP_2017_2100_POP_REFERENCE_Y2020M05D01.CSV` - Population projections
- `GBD_population/` - GBD population data

---

## Output Files

Each script generates 3 output files:

1. **Prediction plot (PDF):** `[Pathogen]_[Counterfactual]_prediction_plot.pdf`
2. **ASR differences (CSV):** `[Pathogen]_[Counterfactual]_asr_differences.csv`
3. **Optimal parameters (CSV):** `[Pathogen]_[Counterfactual]_optimal_parameters.csv`

**Total outputs:** 16 scripts × 3 files = **48 files**

---

## How to Run

### Run a single script
```r
source("Figure6/Figure6_Escherichia_coli_Associated.R")
```

### Run all 16 scripts
```r
figure6_files <- list.files("Figure6", pattern = "^Figure6_.*_.*\\.R$", full.names = TRUE)
for (script in figure6_files) {
  source(script)
}
```

### On Code Ocean
```bash
cd /code/Figure6
Rscript Figure6_Escherichia_coli_Associated.R
```

---

## Analysis Method

### BAPC (Bayesian Age-Period-Cohort) Model

Each script performs:

1. **Data Preparation** (1990-2019)
2. **Parameter Optimization** (Grid search with cross-validation)
3. **Model Training** (Optimal parameters)
4. **Prediction** (2020-2021, counterfactual scenario)
5. **Impact Assessment** (Observed vs predicted)

### Key Features

- Cross-validation for parameter tuning
- Independent hold-out set validation (2017-2019)
- Bayesian uncertainty quantification
- 95% credible intervals

---

## Run Time

- **Single script:** 20-30 minutes
- **All 16 scripts:** 5-8 hours (sequential) or 30-40 minutes (parallel)

---

## Plot Specifications

**Optimized for publication quality:**
- Size: 12 × 8 inches
- Resolution: 300 DPI
- Device: cairo_pdf
- Margins: Optimized for complete axis label display
- Font sizes: Title 16pt, Axis labels 14pt, Tick labels 12pt

---

## Code Availability

All scripts are:
- ✅ Code Ocean compatible
- ✅ Cross-platform (Windows/Mac/Linux)
- ✅ Fully documented
- ✅ Ready for publication

---

**Last Updated:** 2025-10-21  
**Status:** Production ready  
**Language:** English only

