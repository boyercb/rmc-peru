table1 <- 
  map2(
    rep(c("any_physical", "any_sexual"), each = 2),
    rep(c(FALSE, TRUE), 2),
    function(outcome, adjusted) {
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
      z_covs <- z_selected$covariate
      
      fe <- c(
        paste0("strata_new_", 2:4, "_c"),
        paste0("batch_", c(2, 4, 5), "_c")
      )
      covs <- unique(c(y_covs, z_covs, fe))
      
      lm_lin(
        formula = reformulate(
          termlabels = "treatment",
          response = outcome
        ),
        covariates = if (adjusted) {
          reformulate(
            termlabels = covs
          )
        } else {
          reformulate(
            termlabels = fe
          )
        },
        data = subset(rmc, id_status_w == 1 & batch != 3)
      )
    }
  )

covs <- bind_rows(
  postlasso(
    covariates = lassocovs_s2,
    outcome = "any_physical", 
    data = filter(rmc, id_status_w == 1 & strata_new %in% c(2,3) & batch != 3),
    fixed_effects = paste0("batch_", c(2, 4, 5), "_c_s2")
  ),
  postlasso(
    covariates = lassocovs_s2,
    outcome = "any_sexual", 
    data = filter(rmc, id_status_w == 1 & strata_new %in% c(2,3) & batch != 3),
    fixed_effects = paste0("batch_", c(2, 4, 5), "_c_s2")
  )
)

table2 <- 
  map2(
    rep(c("any_physical", "any_sexual"), each = 2),
    rep(c(FALSE, TRUE), 2),
    function(outcome, adjusted) {
      y_covs <- covs$covariate[
        covs$outcome == outcome 
      ]
      z_covs <- NULL
      
      fe <- c(
        paste0("batch_", c(2, 4, 5), "_c_s2")
      )
      covs <- unique(c(y_covs, z_covs, fe))
      
      lm_lin(
        formula = reformulate(
          termlabels = "treatment",
          response = outcome
        ),
        covariates = if (adjusted) {
          reformulate(
            termlabels = covs
          )
        } else {
          reformulate(
            termlabels = fe
          )
        },
        data = subset(rmc, id_status_w == 1 & batch != 3 & strata_new %in% c(2, 3))
      )
    }
  )

covs <- bind_rows(
  postlasso(
    covariates = lassocovs_s2,
    outcome = "attitudes_m", 
    data = filter(rmc, id_status_w == 1 & batch != 3 & strata_new %in% c(2,3)),
    fixed_effects = paste0("batch_", c(2, 4, 5), "_c_s2")
  ),
  postlasso(
    covariates = lassocovs_s2,
    outcome = "consent_index", 
    data = filter(rmc, id_status_w == 1 & batch != 3 & strata_new %in% c(2,3)),
    fixed_effects = paste0("batch_", c(2, 4, 5), "_c_s2")
  )
)

table3 <- 
  map2(
    rep(c("attitudes_m", "consent_index"), each = 2),
    rep(c(FALSE, TRUE), 2),
    function(outcome, adjusted) {
      y_covs <- covs$covariate[
        covs$outcome == outcome & covs$covariate != "gem6_w_bl_NA_o_c_s2_c"
      ]
      
      z_covs <- NULL
      
      fe <- c(
        paste0("batch_", c(2, 4, 5), "_c_s2")
      )
      covs <- unique(c(y_covs, z_covs, fe))
      
      
      lm_lin(
        formula = reformulate(
          termlabels = "treatment",
          response = outcome
        ),
        covariates = if (adjusted) {
          reformulate(
            termlabels = covs
          )
        } else {
          reformulate(
            termlabels = fe
          )
        },
        #data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
        data = subset(rmc, id_status_w == 1 & batch != 3 & strata_new %in% c(2, 3))
      )
    }
  )

make_publication_table(
  models = table1,
  outcomes = c("any_physical", "any_sexual"),
  labels = violence_labels[2:3],
  treatment = "treatment",
  coef_rename = c('treatment' = 'HEP'),
  title = "Effects on violence outcomes excluding Facilitator 3\\label{tab:drop_fac_1}",
  data = rmc
) |> 
  save_kable("HEP-manuscript/tables/drop_facilitator_1.tex")

make_publication_table(
  models = table2,
  outcomes = c("any_physical", "any_sexual"),
  labels = violence_labels[2:3],
  treatment = "treatment",
  coef_rename = c('treatment' = 'HEP'),
  title = "Effects on violence outcomes excluding Facilitator 3, among those couples reporting violence at baseline\\label{tab:drop_fac_2}",
  data = rmc
) |> 
  save_kable("HEP-manuscript/tables/drop_facilitator_2.tex")

make_publication_table(
  models = table3,
  outcomes = c("attitudes_m", "consent_index"),
  labels = c(secondary_labels[7], primary_labels[2]),
  treatment = "treatment",
  coef_rename = c('treatment' = 'HEP'),
  title = "Effects on other possible mechanisms excluding Facilitator 3, among those couples reporting violence at baseline \\label{tab:drop_fac_3}",
  data = rmc
) |> 
  save_kable("HEP-manuscript/tables/drop_facilitator_3.tex")
