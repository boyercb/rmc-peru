
# education ---------------------------------------------------------------

educ_models <- map2(
  rep(c(violence_outcomes[1:3], secondary_outcomes[7:8]), each = 2),
  rep(c(FALSE, TRUE), 5),
  function(outcome, adjusted) {
    strata_FE <- c(
      paste0("batch_", 2:5, "_c_s2")
    )
    
    if (adjusted) {
      y_covs <- y_selected_strata$covariate[
        y_selected_strata$outcome == outcome &
          y_selected_strata$strata == 2
      ]
      z_covs <- z_selected_strata$covariate[
        z_selected_strata$strata == 2
      ]
      
      covs <- unique(c(y_covs, z_covs, strata_FE))
      
    } else {
      covs <- strata_FE
    }
    
    lm_robust(
      formula = reformulate(
        termlabels = c(
          "treatment",
          "I(education_w_bl >= 6)",
          covs,
          paste0("treatment:", covs),
          paste0("treatment:", "I(education_w_bl >= 6)")
        ),
        response = outcome
      ),
      data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
    )
  })


make_subgroup_table(
  models = educ_models[1:6],
  outcomes = violence_outcomes[1:3],
  outcome_labels = violence_labels[1:3],
  treatment = "treatment", 
  subgroup = rmc[rmc$id_status_w == 1 & rmc$strata_new %in% c(2, 3), "education_w_bl"] < 6,
  keep = c("I\\(education", "treatment:I\\(education"),
  coef_rename = c(
    "I(education_w_bl >= 6)TRUE" = "education $\\geq$ median",
    "treatment:I(education_w_bl >= 6)TRUE" = "treatment $\\times$ education $\\geq$ median"
    ),
  title = "ITT estimates of effects of HEP on primary violence outcomes by violence and education \\label{tab:itt_educ}",
  data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
)|>
  save_kable(
    file = "6_tables/itt_educ.tex"
  )


# age ---------------------------------------------------------------------

rmc$age_w_bl_geq_median <- as.numeric(I(rmc$age_w_bl >= median(rmc$age_w_bl, na.rm = TRUE)))

age_models <- map2(
  rep(violence_outcomes[1:3], each = 2),
  rep(c(FALSE, TRUE), 3),
  function(outcome, adjusted) {
    strata_FE <- c(
      paste0("batch_", 2:5, "_c_s2")
    )
    
    if (adjusted) {
      y_covs <- y_selected_strata$covariate[
        y_selected_strata$outcome == outcome &
          y_selected_strata$strata == 2
      ]
      z_covs <- z_selected_strata$covariate[
        z_selected_strata$strata == 2
      ]
      
      covs <- unique(c(y_covs, z_covs, strata_FE))
      
    } else {
      covs <- strata_FE
    }
    
    lm_robust(
      formula = reformulate(
        termlabels = c(
          "treatment",
          "age_w_bl_geq_median",
          covs,
          paste0("treatment:", covs),
          paste0("treatment:", "age_w_bl_geq_median")
        ),
        response = outcome
      ),
      data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
    )
  })


make_subgroup_table(
  models = age_models,
  outcomes = violence_outcomes[1:3],
  outcome_labels = violence_labels[1:3],
  treatment = "treatment", 
  subgroup = rmc[rmc$id_status_w == 1 & rmc$strata_new %in% c(2, 3), "age_w_bl_geq_median"] == 0,
  keep = c("age_w_bl_geq_median", "treatment:age_w_bl_geq_median"),
  coef_rename = c(
    "age_w_bl_geq_median" = "age $\\geq$ median",
    "treatment:age_w_bl_geq_median" = "treatment $\\times$ age $\\geq$ median"
  ),
  title = "ITT estimates of effects of HEP on primary violence outcomes by violence and age \\label{tab:itt_age}",
  data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
)|>
  save_kable(
    file = "6_tables/itt_age.tex"
  )


# alcohol -----------------------------------------------------------------

alcohol_models <- map2(
  rep(violence_outcomes[1:3], each = 2),
  rep(c(FALSE, TRUE), 3),
  function(outcome, adjusted) {
    strata_FE <- c(
      paste0("batch_", 2:5, "_c_s2")
    )
    
    if (adjusted) {
      y_covs <- y_selected_strata$covariate[
        y_selected_strata$outcome == outcome &
          y_selected_strata$strata == 2
      ]
      z_covs <- z_selected_strata$covariate[
        z_selected_strata$strata == 2
      ]
      
      covs <- unique(c(y_covs, z_covs, strata_FE))
      
    } else {
      covs <- strata_FE
    }
    
    lm_robust(
      formula = reformulate(
        termlabels = c(
          "treatment",
          "I(alcohol_man_w_bl > 1)",
          covs,
          paste0("treatment:", covs),
          paste0("treatment:", "I(alcohol_man_w_bl > 1)")
        ),
        response = outcome
      ),
      data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
    )
  })

make_subgroup_table(
  models = alcohol_models,
  outcomes = violence_outcomes[1:3],
  outcome_labels = violence_labels[1:3],
  treatment = "treatment", 
  subgroup = rmc[rmc$id_status_w == 1 & rmc$strata_new %in% c(2, 3), "alcohol_man_w_bl"] == 1,
  keep = c("I\\(alcohol", "treatment:I\\(alcohol"),
  coef_rename = c(
    "I(alcohol_man_w_bl > 1)TRUE" = "man drinks",
    "treatment:I(alcohol_man_w_bl > 1)TRUE" = "treatment $\\times$ man drinks"
  ),
  title = "ITT estimates of effects of HEP on primary violence outcomes by violence and alcohol use \\label{tab:itt_alcohol}",
  data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
)|>
  save_kable(
    file = "6_tables/itt_alcohol.tex"
  )


# additional subgroups of interest ----------------------------------------

add_subgroups <- pmap(
  list(
    subgroup = rep(c("education_w_bl", "alcohol_man_w_bl"), each = 2),
    adjusted = c(FALSE, TRUE, FALSE, TRUE)
  ),
  function(subgroup, adjusted) {
    strata_FE <- c(
      paste0("batch_", 2:5, "_c_s2")
    )
    
    if (adjusted) {
      y_covs <- y_selected_strata$covariate[
        y_selected_strata$outcome == "attitudes_m" &
          y_selected_strata$strata == 2
      ]
      z_covs <- z_selected_strata$covariate[
        z_selected_strata$strata == 2
      ]
      
      covs <- unique(c(y_covs, z_covs, strata_FE))
      
    } else {
      covs <- strata_FE
    }
    
    
    lm_robust(
      formula = reformulate(
        termlabels = c(
          "treatment",
          covs,
          paste0("treatment:", covs)
        ),
        response = "attitudes_m"
      ),
      data = if (subgroup == "education_w_bl") {
        subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3) & I(education_w_bl < 6))
      } else {
        subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3) & I(alcohol_man_w_bl > 1))
      }
    )
  }
)

make_report_table(
  models = add_subgroups[1:2],
  outcomes = secondary_outcomes[7],
  outcome_labels = secondary_labels[7],
  treatment = "treatment", 
  title = "ITT estimates of effects of HEP on Men's attitudes among couples reporting violence and lower educational attainment at baseline \\label{tab:itt_educ}",
  data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3) & I(education_w_bl < 6))
)|>
  save_kable(
    file = "6_tables/itt_educ_norms.tex"
  )

make_report_table(
  models = add_subgroups[3:4],
  outcomes = secondary_outcomes[7],
  outcome_labels = secondary_labels[7],
  treatment = "treatment", 
  title = "ITT estimates of effects of HEP on Men's attitudes among couples reporting violence and the man drinks at baseline \\label{tab:itt_educ}",
  data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3) & I(alcohol_man_w_bl > 1))
)|>
  save_kable(
    file = "6_tables/itt_alcohol_norms.tex"
  )

