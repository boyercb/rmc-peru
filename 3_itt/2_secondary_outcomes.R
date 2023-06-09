# estimate ITT effects ----------------------------------------------------

itt_secondary <- 
  map2(
    rep(secondary_outcomes, each = 2),
    rep(c(FALSE, TRUE), length(secondary_outcomes)),
    function(outcome, adjusted) {
      strata_FE <- paste0("strata_new_", 2:7, "_c")
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
      #r_covs <- r_selected$covariate[r_selected$outcome == outcome]
      z_covs <- z_selected$covariate[z_selected$outcome == outcome]
      covs <- unique(c(y_covs, z_covs, strata_FE))
      
      lm_robust(
        reformulate(
          termlabels = if (adjusted & length(covs) > 0) {
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

names(itt_secondary) <- 
  c(paste0("(", 1:4, ")"), 
    paste0("(", 1:4, ")"), 
    paste0("(", 1:4, ")"), 
    paste0("(", 1:4, ")"),
    paste0("(", 1:2, ")"),
    paste0("(", 1:2, ")"))

make_report_table(
  models = itt_secondary[1:4],
  outcomes = secondary_outcomes[1:2],
  outcome_labels = secondary_labels[1:2],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_satisfaction.tex"
  )

make_report_table(
  models = itt_secondary[5:8],
  outcomes = secondary_outcomes[3:4],
  outcome_labels = secondary_labels[3:4],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_perc_satisfaction.tex"
  )

make_report_table(
  models = itt_secondary[9:12],
  outcomes = secondary_outcomes[5:6],
  outcome_labels = secondary_labels[5:6],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_conflict.tex"
  )

make_report_table(
  models = itt_secondary[13:16],
  outcomes = secondary_outcomes[7:8],
  outcome_labels = secondary_labels[7:8],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_attitudes.tex"
  )

make_report_table(
  models = itt_secondary[17:18],
  outcomes = secondary_outcomes[9],
  outcome_labels = secondary_labels[9],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_depression.tex"
  )

make_report_table(
  models = itt_secondary[19:20],
  outcomes = secondary_outcomes[10],
  outcome_labels = secondary_labels[10],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_depression.tex"
  )




