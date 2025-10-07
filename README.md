Institution or Major? — Repo Overview

This repository contains code for generating the figures and tables in the paper “Institution or Major? Understanding Student Preferences in College Admissions” (Chen, Chen, Yang, and Hung). The project is organized into four top‑level folders that mirror the workflow: source code, data cleaning, analysis, and outputs. 

## Structure
- `00src/`
  - Core R scripts and helpers used across analyses.
  - Notable files: `visualization.R` (plot helpers), `MLE_Cov.R`, `UHetero_MLE.R`, `DChoice_matrices.R`, `cluster_table.R`, `new_verify.R`.
- `02CleanData/`
  - Data cleaning and summary notebooks.
  - Notable: `MajorSummary.Rmd` for constructing summary statistics used downstream.
- `03Analysis/`
  - Main analysis scripts that load prepared data and produce paper results.
  - Notable files: `01DataPrepare.R`, `02main_results.R`, `04cov_results.R`, `05Saturated.R`, `06Appendix.R`, `Main_Summary.R`, `Result_visualization.R`.
- `04Output/`
  - Generated artifacts organized by type.
  - Subfolders: `Figure/` (final figures), `Tables/` (final tables), `Sample-Figure/` (examples/drafts).

## Usage Notes
- Run cleaning in `01DataPrepare.R` first, then analysis in `02main_results.R`.
- Scripts in `02main_results.R` source utilities from `00src/` and write to `04Output/`.
- `04cov_results.R`, `05Saturated.R`, `06Appendix.` are used for robustness checks.
- Figures and tables for the paper are saved under `04Output/Figure/` and `04Output/Tables/`.
- Data and figures are not version-controlld. `04Output/Sample-Figure` provides samples of the figures generated.

## Contributors
- Hao-Hung Yang: `MLE_Cov.R`, `UHetero_MLE.R`
- Chi-Chao Hung: All other codes.

