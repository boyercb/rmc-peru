

# global options ----------------------------------------------------------

impute <- FALSE


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

source("1_cleaning/4_impute.R")

source("1_cleaning/5_outcomes.R")


# covariate selection -----------------------------------------------------

source("2_covariate_selection/0_outcome_lists.R")

source("2_covariate_selection/1_select_itt.R")

source("2_covariate_selection/2_select_compliance.R")

source("2_covariate_selection/3_select_attrition.R")

source("2_covariate_selection/4_select_stratified.R")

source("2_covariate_selection/5_combine_selections.R")


# intent-to-treat ---------------------------------------------------------

source("3_itt/0_draw_randomizations.R")

source("3_itt/1_prereg_outcomes.R")

source("3_itt/2_prereg_subgroups.R")

source("3_itt/3_peer_effects.R")

source("3_itt/4_peer_mechanisms.R")

source("3_itt/5_fac_heterogeneity.R")



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

