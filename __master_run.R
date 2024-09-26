
# TODO:
# [x] fix group randomization issue identified by JP and re-run results
# - re-estimate tables 9-10 with new coding that JP sent
# -NOT add group composition variables to lasso: 
# [x] create figure plotting facilitator and group-level effects
# - we should make Figure 3 analogous to Figure 4. 
#   Can you add (for now, for comparison), a version of Figure 4 that shows 
#   non-parametric relationship between sexual violence (rather than remain in 
#   the chat) and proportion of men who justify violence?
# 
# - Add both key regressors in Tables 6 & 7 (justification index value and proportion any just) match the x-axis variable in Figures 3 and 4 


# packages and functions --------------------------------------------------

source("0_bin/0_packages.R")

source("0_bin/1_helper_functions.R")

source("0_bin/2_cleanning_functions.R")

source("0_bin/3_lasso_functions.R")

source("0_bin/4_analysis_functions.R")

source("0_bin/5_table_functions.R")

source("0_bin/6_plot_functions.R")


# data cleaning -----------------------------------------------------------

source("1_cleaning/0_load_data.R")

source("1_cleaning/1_clean.R")

source("1_cleaning/2_merge.R")

source("1_cleaning/3_covariates.R")

# source("1_cleaning/4_impute.R")

source("1_cleaning/5_outcomes.R")


# covariate selection -----------------------------------------------------

source("2_covariate_selection/0_outcome_lists.R")

source("2_covariate_selection/1_select_itt.R")

source("2_covariate_selection/2_select_compliance.R")

source("2_covariate_selection/3_select_attrition.R")

source("2_covariate_selection/4_select_stratified.R")


# intent-to-treat ---------------------------------------------------------

source("3_itt/1_primary_outcomes.R")

source("3_itt/2_secondary_outcomes.R")

source("3_itt/3_item_plots.R")

source("3_itt/4_subgroups_ipv.R")

source("3_itt/5_subgroups_other.R")

source("3_itt/6_subgroup_plots.R")

source("3_itt/7_group_level_exposures.R")

source("3_itt/8_make_publication_tables.R")


# new analysis ------------------------------------------------------------

source("3_itt/fac_heterogeneity_1.R")

source("3_itt/fac_heterogeneity_2.R")

source("3_itt/coded_message_histograms.R")

source("3_itt/coded_messages_mechanisms_table.R")


# compliance --------------------------------------------------------------

source("4_compliance/iv_estimates.R")

# robustness --------------------------------------------------------------


source("5_robustness/attrition.R")

source("5_robustness/demand_effects.R")

