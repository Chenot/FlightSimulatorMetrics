# Assessing and visualizing pilot performance in traffic patterns: A composite score approach
- **Journal and DOI:** Safety [[link to DOI](https://doi.org/10.3390/safety11020037)]
- **Authors:** Quentin Chenot, Florine Riedinger, Frédéric Dehais and Sébastien Scannella
- **Date of publication:** 2025-04-23 [YYYY-MM-DD]

This repository contains R scripts for the analysis conducted in this project. The scripts are organized to correspond with the sections of the associated scientific article.

## **Directory Structure**

- **`Data/`**: Contains the data files (`df_final.csv`, `simu_perf_processed.csv`, `app_questionnaire.csv`).
- **`Figures/`**: Where the generated figures will be saved.
- **`Scripts/`**: Contains all the scripts (`data_loading.R`, `utils.R`, and the analysis scripts).
- **`ShinyApps/`**: Contains all the scripts and data for the web app.

## **Scripts Overview**

1. **Data Loading (`Scripts/data_loading.R`)**
   - Sets up the working environment and directories.
   - Loads datasets from the `Data/` directory.

2. **Utility Functions (`Scripts/utils.R`)**
   - Loads or install required packages.
   - Contains utility functions used across multiple scripts to avoid code duplication.

3. **Demographics Analysis (`Scripts/demographics_analysis.R`)**
   - Performs demographic analyses as described in Section 2.1 of the article.
   - Generates a summary paragraph.

4. **Hypothesis 1: Scenario Workload (`Scripts/hypothesis1_scenario_workload.R`)**
   - Tests Hypothesis 1 from Section 2.6.1.
   - Analyzes how scenario difficulty and flight rules affect workload measures.
   - Generates plots and summary paragraphs.

5. **Hypothesis 2: Subjective vs. Objective Workload (`Scripts/hypothesis2_subjective_objective_workload.R`)**
   - Tests Hypothesis 2 from Section 2.6.2.
   - Explores the relationship between subjective (NASA-TLX) and objective (Oddball) workload measures.
   - Generates plots and summary paragraphs.

6. **Hypothesis 3: Composite Score & Workload (`Scripts/hypothesis3_composite_score_workload.R`)**
   - Tests Hypothesis 3 from Section 2.6.3.
   - Examines the relationship between composite performance scores and workload measures.
   - Generates plots and summary paragraphs.

7. **Questionnaire analysis (`questionnaire_analysis.R`)**
   - Performs questionnaire analyses as described in the section 3.4 of the article.
   - Generates the table and summary paragraph.

8. **Supplementary analyses (`Scripts/supplementary_material.R`)**
   - Performs analyses as described in the Supplementary Analyses and in Supplementary Material.
   - Examines the relationship between flights hours and workload.
   - Examines the relationship between flights hours and composite score.
   - Examines the sensitivity of the composite score per scenario.
   - Generates the associated plots.

## **Instructions for Running the Scripts**

1. **Run the Scripts**
   - Navigate to the `Scripts/` directory.
   - Run the scripts in order if performing all analyses (statistical, graphs, text), or run individual scripts as needed.

## **Dependencies**

### Statistical Analysis Software
- R and RStudio
- Required packages: `dplyr`, `tidyverse`, `lmerTest`, `lme4`, `broom`, `ggplot2`, `ggpubr`, `gghalves`, `PupillometryR`, `performance`, `gridExtra`, `e1071`, `rstudioapi`, `nortest`, `xtable`, `reshape2`, `tidyr`, `kableExtra`.

### Computational environment
- OS: Windows 10
- R version: 4.2.2
- RStudio version: 2023.06.2

## **Contact**
For questions or issues, please contact Quentin Chenot [quentinchenot@gmail.com].

