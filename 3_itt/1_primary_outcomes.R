

# estimate ITT effects ----------------------------------------------------

itt_violence <- 
  map2(
    rep(violence_outcomes, each = 2),
    rep(c(FALSE, TRUE), length(violence_outcomes)),
    function(outcome, adjusted) {
      strata_FE <- c(
        paste0("strata_new_", 2:4, "_c"),
        paste0("batch_", 2:6, "_c")
        # paste0("strata_new_2_c:batch_", 2:5, "_c"),
        # paste0("strata_new_3_c:batch_", 2:5, "_c")
      )
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
      # r_covs <- r_selected$covariate
      z_covs <- z_selected$covariate
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
        #weights = response_weights[[outcome]]
      )      
    }
  )

itt_time_to_violence <- 
  map2(
    rep(time_to_violence_outcomes, each = 2),
    rep(c(FALSE, TRUE), length(time_to_violence_outcomes)),
    function(outcome, adjusted) {
      strata_FE <- c(
        paste0("strata_new_", 2:4, "_c"),
        paste0("batch_", 2:6, "_c")
      )
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
      # r_covs <- r_selected$covariate
      z_covs <- z_selected$covariate
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
      strata_FE <- c(
        paste0("strata_new_", 2:4, "_c"),
        paste0("batch_", 2:6, "_c")
      )
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
      # r_covs <- r_selected$covariate
      z_covs <- z_selected$covariate
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
    paste0("(", 1:6, ")"),
    paste0("(", 1:6, ")"),
    paste0("(", 1:6, ")"),
    paste0("(", 1:6, ")"),
    paste0("(", 1:6, ")"),
    paste0("(", 1:6, ")"),
    paste0("(", 1:4, ")"))

make_report_table(
  models = itt_violence[1:6],
  outcomes = violence_outcomes[1:3],
  outcome_labels = violence_labels[1:3],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on primary violence outcomes \\label{tab:itt_violence_bin_1}",
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
  title = "ITT estimates of effects of HEP on other binary violence outcomes \\label{tab:itt_violence_bin_2}",
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
  title = "ITT estimates of effects of HEP on continuous violence indices \\label{tab:itt_violence_cont_1}",
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
  title = "ITT estimates of effects of HEP on other continuous violence indices \\label{tab:itt_violence_cont_2}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_violence_cont_2.tex"
  )

make_report_table(
  models = itt_violence[25:30],
  outcomes = violence_outcomes[13:15],
  outcome_labels = violence_labels[13:15],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on any response $\\geq$ ``Sometimes'' \\label{tab:itt_violence_bin_sometimes}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_violence_bin_sometimes.tex"
  )

make_report_table(
  models = itt_violence[31:36],
  outcomes = violence_outcomes[16:18],
  outcome_labels = violence_labels[16:18],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on any response $\\geq$ ``Often'' \\label{tab:itt_violence_bin_often}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_violence_bin_often.tex"
  )

make_report_table(
  models = itt_violence[37:42],
  outcomes = violence_outcomes[19:21],
  outcome_labels = violence_labels[19:21],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on breadth of violence (i.e. proportion of possible acts with a positive response) \\label{tab:itt_violence_breadth}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_violence_breadth.tex"
  )

make_report_table(
  models = itt_violence[43:48],
  outcomes = violence_outcomes[22:24],
  outcome_labels = violence_labels[22:24],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on continuous violence z-scores \\label{tab:itt_violence_z_1}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_violence_z_1.tex"
  )

make_report_table(
  models = itt_violence[49:54],
  outcomes = violence_outcomes[25:27],
  outcome_labels = violence_labels[25:27],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on other continuous violence z-scores \\label{tab:itt_violence_z_2}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_violence_z_2.tex"
  )

make_report_table(
  models = itt_violence[55:58],
  outcomes = violence_outcomes[28:29],
  outcome_labels = violence_labels[28:29],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on violence severity where ``Moderate'' = single push or slap and ``Severe'' = other acts or multiple push or slaps. \\label{tab:itt_violence_severe}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_violence_severe.tex"
  )


names(itt_primary) <- paste0("(", 1:6, ")")

make_report_table(
  models = itt_primary,
  outcomes = primary_outcomes,
  outcome_labels = primary_labels,
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on other primary outcomes \\label{tab:itt_primary}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_primary.tex"
  )
