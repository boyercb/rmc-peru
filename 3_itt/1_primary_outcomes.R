

# estimate ITT effects ----------------------------------------------------

itt_violence <- 
  map2(
    rep(violence_outcomes, each = 2),
    rep(c(FALSE, TRUE), length(violence_outcomes)),
    function(outcome, adjusted) {
      strata_FE <- paste0("strata_new_", 2:7, "_c")
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
      #r_covs <- r_selected$covariate[r_selected$outcome == outcome]
      z_covs <- z_selected$covariate[z_selected$outcome == outcome]
      covs <- unique(c(y_covs, z_covs, strata_FE))
      
      lm_robust(
        reformulate(
          termlabels = if (adjusted) {
            c("treatment", covs, paste0("treatment:", covs))
          } else {
            c("treatment", strata_FE, paste0("treatment:", strata_FE))
          },
          response = outcome,
        ),
        data = filter(rmc, id_status_w == 1)
      )      
    }
  )

itt_time_to_violence <- 
  map2(
    rep(time_to_violence_outcomes, each = 2),
    rep(c(FALSE, TRUE), length(time_to_violence_outcomes)),
    function(outcome, adjusted) {
      strata_FE <- paste0("strata_new_", 2:7, "_c")
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
      #r_covs <- r_selected$covariate[r_selected$outcome == outcome]
      z_covs <- z_selected$covariate[z_selected$outcome == outcome]
      covs <- unique(c(y_covs, z_covs, strata_FE))
      
      lm_robust(
        reformulate(
          termlabels = if (adjusted) {
            c("treatment", covs, paste0("treatment:", covs))
          } else {
            c("treatment", strata_FE, paste0("treatment:", strata_FE))
          },
          response = outcome,
        ),
        data = filter(rmc, id_status_w == 1)
      )      
    }
  )

itt_primary <- 
  map2(
    rep(primary_outcomes, each = 2),
    rep(c(FALSE, TRUE), length(primary_outcomes)),
    function(outcome, adjusted) {
      strata_FE <- paste0("strata_new_", 2:7, "_c")
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
      #r_covs <- r_selected$covariate[r_selected$outcome == outcome]
      z_covs <- z_selected$covariate[z_selected$outcome == outcome]
      covs <- unique(c(y_covs, z_covs, strata_FE))
      
      lm_robust(
        reformulate(
          termlabels = if (adjusted) {
            c("treatment", covs, paste0("treatment:", covs))
          } else {
            c("treatment", strata_FE, paste0("treatment:", strata_FE))
          },
          response = outcome,
        ),
        data = filter(rmc, id_status_w == 1)
      )      
    }
  )


# make tables -------------------------------------------------------------

names(itt_violence) <- 
  c(paste0("(", 1:6, ")"), 
    paste0("(", 1:6, ")"), 
    paste0("(", 1:6, ")"),
    paste0("(", 1:6, ")"))

make_report_table(
  models = itt_violence[1:6],
  outcomes = violence_outcomes[1:3],
  outcome_labels = violence_labels[1:3],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_violence_bin_1.tex"
  )

make_report_table(
  models = itt_violence[7:12],
  outcomes = violence_outcomes[4:6],
  outcome_labels = violence_labels[4:6],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_violence_bin_2.tex"
  )

make_report_table(
  models = itt_violence[13:18],
  outcomes = violence_outcomes[7:9],
  outcome_labels = violence_labels[7:9],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_violence_cont_1.tex"
  )

make_report_table(
  models = itt_violence[19:24],
  outcomes = violence_outcomes[10:12],
  outcome_labels = violence_labels[10:12],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_violence_cont_2.tex"
  )




names(itt_primary) <- paste0("(", 1:6, ")")

make_report_table(
  models = itt_primary,
  outcomes = primary_outcomes,
  outcome_labels = primary_labels,
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_primary.tex"
  )
