# rmc-peru

Replication files for: 

Boyer, C., Field, E., Lehrer, R., Morrison, A., & Piras, C. (2025). “Guy Talk: Catalyzing Peer Effects on IPV through Virtual Support Groups for Men”. https://sites.duke.edu/ericafield/files/2025/03/wp_2025_03_Guy_Talk_Peru.pdf

# Abstract
We experimentally evaluate a novel approach to IPV prevention that harnesses social media
to recruit and engage men in a virtual support group delivered by trained male facilitators via WhatsApp. The program succeeded in recruiting men at high risk of committing IPV through
self-targeting alone: 52% of partners of men who enroll in the program in response to social
media ads report experiencing IPV at baseline, more than four times the national average and
nearly twice the rates observed in men recruited through targeted and untargeted invitations.
Moreover, on average, participation in the program reduced the probability that female partners report sexual violence at endline by 20%. Treatment effects are concentrated among younger men (-36%), men who exhibit violence at baseline (-27%), and among those whose wives report that they do not drink alcohol (-40%). Program effects are also highly sensitive to group composition,which was randomly assigned. Segregating individuals based on baseline risk appears to magnify program impacts on high-risk individuals, and hence the program impact overall.

## Requirements

This code requires R version 4.0 or higher. The following R packages are required:

- `tidyverse` - collection of tidy data packages for data manipulation and visualization
- `recipes` - for data preprocessing and feature engineering
- `haven` - for reading and writing SPSS, Stata, and SAS files
- `hdm` - for high-dimensional econometric models
- `estimatr` - for fast estimation with robust standard errors
- `modelsummary` - for creating publication-ready tables
- `kableExtra` - for enhanced table formatting
- `mice` - for multiple imputation of missing data
- `grf` - for generalized random forests and causal inference
- `sandwich` - for robust covariance matrix estimation
- `lmtest` - for testing linear regression models
- `randomizr` - for random assignment procedures
- `splines` - for spline basis functions

## Installation

1. Clone this repository:
```bash
git clone https://github.com/yourusername/rmc-peru.git
cd rmc-peru
```

2. Install required R packages:
```r
install.packages(c("tidyverse", "recipes", "haven", "hdm", "estimatr", 
                   "modelsummary", "kableExtra", "mice", "grf", "sandwich", 
                   "lmtest", "randomizr", "splines"))
```

3. Create the data directory and place your raw data files in the appropriate subdirectory:
```r
dir.create("__data/RMC", recursive = TRUE, showWarnings = FALSE)
```

## Repository Structure

- `0_bin/` - Contains all helper functions and package loading
  - `0_packages.R` - Package dependencies
  - `1_helper_functions.R` - General utility functions
  - `2_cleanning_functions.R` - Data cleaning functions
  - `3_lasso_functions.R` - LASSO regression functions
  - `4_analysis_functions.R` - Statistical analysis functions
  - `5_table_functions.R` - Table generation functions
  - `6_plot_functions.R` - Plotting functions
- `1_cleaning/` - Data processing pipeline
  - `0_load_data.R` - Load raw data files
  - `1_clean.R` - Initial data cleaning
  - `2_merge.R` - Merge datasets
  - `3_covariates.R` - Create covariate variables
  - `4_impute.R` - Handle missing data
  - `5_outcomes.R` - Prepare outcome variables
- `2_covariate_selection/` - Variable selection procedures
  - `0_outcome_lists.R` - Define outcome variable lists
  - `1_select_itt.R` - Select covariates for ITT analysis
  - `2_select_compliance.R` - Select covariates for compliance analysis
  - `3_select_attrition.R` - Select covariates for attrition analysis
  - `4_select_stratified.R` - Select covariates for stratified analysis
  - `5_combine_selections.R` - Combine covariate selections
- `3_itt/` - Intent-to-treat analysis
  - `0_draw_randomizations.R` - Generate permutted assignments for randomization inference
  - `1_prereg_outcomes.R` - Pre-registered primary outcomes
  - `2_prereg_subgroups.R` - Pre-registered subgroup analyses
  - `3_peer_effects.R` - Peer effects analysis
  - `4_peer_mechanisms.R` - Peer mechanism analysis
  - `5_subgroups_*.R` - Various subgroup analyses
  - `8_make_publication_tables.R` - Generate final tables
- `4_compliance/` - Compliance analysis (instrumental variables)
- `5_robustness/` - Robustness checks
- `6_tables/` - Generated LaTeX tables
- `7_figures/` - Generated figures
- `8_report/` - Report generation files
- `__data/` - Data directory (not included in repository)
- `__archive/` - Archived analysis files

## How to run

### Running the complete analysis

To reproduce all results from the study, run the master script in R from the project root directory:

```r
source("__master_run.R")
```

**Note:** Requires the raw data (not included). The complete analysis pipeline processes survey data, performs multiple imputation, conducts covariate selection using LASSO, and runs the full statistical analysis. This may take several hours to complete depending on your system and the size of the dataset.

### Running individual components

You can also run individual parts of the analysis:

```r
# Load packages and functions
source("0_bin/0_packages.R")
source("0_bin/1_helper_functions.R")
# ... load other function files as needed

# Run data cleaning only
source("1_cleaning/0_load_data.R")
source("1_cleaning/1_clean.R")
# ... continue with other cleaning steps

# Run specific analyses
source("3_itt/1_prereg_outcomes.R")  # Primary outcomes
source("3_itt/2_prereg_subgroups.R") # Subgroup analyses
```

### Configuration options

The analysis includes several configuration options that can be set at the top of `__master_run.R`:

- `impute <- FALSE` - Set to `TRUE` to perform multiple imputation of missing data (not performed in final analysis).

## Analysis Components

The analysis includes:

1. **Data Cleaning** - Processing of baseline and endline survey data for men and their female partners
2. **Covariate Selection** - LASSO-based selection of control variables for different analyses
3. **Intent-to-Treat Analysis** - Primary analysis comparing treatment and control groups
4. **Subgroup Analyses** - Analysis by baseline characteristics including IPV propensity, education, and alcohol use
5. **Peer Effects** - Analysis of spillover effects within groups
6. **Compliance Analysis** - Instrumental variables estimation accounting for treatment compliance
7. **Robustness Checks** - Tests for attrition bias and demand effects

## Output

The analysis generates:
- Summary statistics and balance tables
- Treatment effect estimates for all primary and secondary outcomes
- Subgroup analysis results
- Robustness check results
- LaTeX formatted tables (saved to `6_tables/` directory)
- Figures and plots (saved to `7_figures/` directory)

## Citation

If you use this code, please cite:

Boyer, C., Field, E., Lehrer, R., Morrison, A., & Piras, C. (2025). “Guy Talk: Catalyzing Peer Effects on IPV through Virtual Support Groups for Men”. https://sites.duke.edu/ericafield/files/2025/03/wp_2025_03_Guy_Talk_Peru.pdf
