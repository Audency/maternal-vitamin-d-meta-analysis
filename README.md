# Maternal Vitamin D Status and Fetal Growth / Adiposity: A Systematic Review and Meta-Analysis

## Overview

Systematic review with meta-analysis investigating the association between maternal vitamin D status (serum 25(OH)D) and fetal growth outcomes, including small-for-gestational-age (SGA), fetal growth restriction (FGR), fetal biometry, estimated fetal weight (EFW), and adiposity.

- **26 studies** (2010--2025) from 16 countries
- **26,542 participants** total
- **Primary outcome:** SGA/FGR (k=4; pooled OR = 1.95, 95% CI 1.47--2.59)
- **Secondary outcome:** Fetal biometry/EFW (k=4; pooled OR = 1.16, 95% CI 1.09--1.23)
- **RCT continuous outcomes:** Femur length, humerus length, EFW, BPD (2 trials)

## Repository Structure

```
.
├── vitaminD_meta_analysis_FINAL.R          # Main analysis script (v6)
├── vitaminD_meta_analysis_v2.R             # Prior version (v2)
├── vitaminD RS Isabel versao boa .R        # Prior version (v4)
├── vitaminD RS Isabel versao boa __Final.R # Prior version (v5)
├── .gitignore
│
└── Resultados/
    ├── figures_v6/                         # Final figures (PNG, 300 dpi)
    │   ├── Forest_SGA_FGR.png
    │   ├── Forest_Biometry_EFW.png
    │   ├── Forest_RCT_MD.png
    │   ├── StudyCharacteristics.png
    │   ├── LabMethods_VitD.png
    │   ├── BubblePlot.png
    │   ├── Outcome_CutoffSummary.png
    │   ├── LOO_Sensitivity.png
    │   ├── Funnel_TrimFill.png
    │   ├── Quality_JBI_NOS.png
    │   ├── Cutoff_Subgroup.png
    │   ├── Narrative_Synthesis.png
    │   └── GRADE_Profile.png
    │
    ├── TableS1_DataExtraction.csv
    ├── TableS2_MetaAnalysisSummary.csv
    ├── TableS4_JBI_NOS_Quality.csv
    ├── TableS_TrimFill.csv
    ├── SR_MasterData_AllStudies.csv
    ├── SR_MetaData_SGA_FGR.csv
    ├── SR_MetaData_Biometry_EFW.csv
    └── SR_LOO_Sensitivity.csv
```

## Key Figures

### Forest Plot -- SGA/FGR (Primary Outcome)
![Forest SGA/FGR](Resultados%20/figures_v6/Forest_SGA_FGR.png)

### Forest Plot -- Fetal Biometry/EFW (Secondary Outcome)
![Forest Biometry](Resultados%20/figures_v6/Forest_Biometry_EFW.png)

### Forest Plot -- RCT Continuous Outcomes (Mean Difference)
![Forest RCT](Resultados%20/figures_v6/Forest_RCT_MD.png)

### Study Characteristics
![Study Characteristics](Resultados%20/figures_v6/StudyCharacteristics.png)

## Methods

- **Model selection:** Fixed-effects when I^2 < 50%; random-effects (REML) when I^2 >= 50%
- **Dichotomous outcomes:** Odds ratios with inverse-variance weighting
- **Continuous outcomes:** Mean difference (MD)
- **Quality assessment:** JBI Critical Appraisal (all studies) + Newcastle-Ottawa Scale (observational)
- **Publication bias:** Egger/Begg tests + Duval & Tweedie trim-and-fill
- **Certainty of evidence:** GRADE framework
- **Sensitivity analyses:** Leave-one-out; subgroup by vitamin D cutoff; cohort-only

## How to Run

### Requirements

R (>= 4.0) with the following packages:

```r
meta, metafor, tidyverse, ggplot2, patchwork, scales,
forcats, tidyr, readr, maps, grid
```

### Execution

```r
source("vitaminD_meta_analysis_FINAL.R")
```

The script will:
1. Install any missing packages automatically
2. Run all meta-analyses and print results to the console
3. Generate all figures (PNG + TIFF at 300 dpi) in `Resultados/figures_v6/`
4. Export supplementary tables as CSV in `Resultados/`

## Authors
- Audencio Victor 

## License

This repository contains analysis code and results for an academic manuscript under preparation. Please contact the authors before reusing.
