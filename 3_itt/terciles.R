itt_violence <- 
  map2(
    rep(c("any_ipv", "any_physical", "any_sexual"), each = 4),
    rep(c(0, 1, 2, 3), 3),
    function(outcome, adjusted) {
      main_estimator(
        outcome = outcome, 
        covariates = if (adjusted == 0) {
          NULL 
        } else if (adjusted == 1) {
          c("age_m_cat2_bl", "age_m_cat3_bl")
        } else if (adjusted == 2) {
          c("age_m_cat2_bl_c", "age_m_cat3_bl_c")
        } else {
          y_covs <- y_selected$covariate[y_selected$outcome == outcome]
          z_covs <- z_selected$covariate
          unique(c(y_covs, z_covs))
        },
        data = filter(rmc, id_status_w == 1)
      )      
    }
  )



names(itt_violence) <- paste0("(", rep(1:4, 3), ")")

make_report_table(
  models = itt_violence[1:4],
  outcomes = "any_ipv",
  outcome_labels = violence_labels[1],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP",
  rows = tibble(
    covariates = c("No", "Age Ter.", "Age Ter. (cent.)", "Yes"),
    FE = c("Yes", "Yes", "Yes", "Yes")
    ),
  data = rmc
) 

make_report_table(
  models = itt_violence[5:8],
  outcomes = "any_ipv",
  outcome_labels = violence_labels[2],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP",
  rows = tibble(
    covariates = c("No", "Age Ter.", "Age Ter. (cent.)", "Yes"),
    FE = c("Yes", "Yes", "Yes", "Yes")
  ),
  data = rmc
) 

make_report_table(
  models = itt_violence[9:12],
  outcomes = "any_ipv",
  outcome_labels = violence_labels[3],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP",
  rows = tibble(
    covariates = c("No", "Age Ter.", "Age Ter. (cent.)", "Yes"),
    FE = c("Yes", "Yes", "Yes", "Yes")
  ),
  data = rmc
) 

modelsummary(
  models = main_estimator(
    outcome = "I(id_status_w == 1)", 
    covariates = c("age_m_cat2_bl", "age_m_cat3_bl"),
    data = rmc
  )  ,
  output = 'latex',
  statistic = c('({std.error})','[{p.value}]'),
  coef_omit = "_c$",
  coef_rename = c(
    "age_m_cat2_bl" = 'Age > 31 and Age <= 38',
    "age_m_cat3_bl" = 'Age > 38 and Age <= 60'
    ),
  gof_omit = "(AIC)|(BIC)|(RMSE)",
  align = paste0('l', 'd'),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  escape = FALSE
)

 
modelsummary(
  models = main_estimator(
    outcome = "remained_in_chat", 
    treatment = c("age_m_cat2_bl", "age_m_cat3_bl"),
    data = subset(rmc, treatment == 1)
  )   ,
  output = 'latex',
  statistic = c('({std.error})','[{p.value}]'),
  coef_omit = "_c$",
  coef_rename = c(
    "age_m_cat2_bl" = 'Age > 31 and Age <= 38',
    "age_m_cat3_bl" = 'Age > 38 and Age <= 60'
  ),
  gof_omit = "(AIC)|(BIC)|(RMSE)",
  align = paste0('l', 'd'),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  escape = FALSE
)


  # save_kable(
  #   file = "6_tables/itt_primary.tex"
  # )