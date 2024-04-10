# estimate ITT effects ----------------------------------------------------

itt_secondary <- 
  map2(
    rep(secondary_outcomes, each = 2),
    rep(c(FALSE, TRUE), length(secondary_outcomes)),
    function(outcome, adjusted) {
      strata_FE <- c(
        paste0("strata_new_", 2:4, "_c"),
        paste0("batch_", 2:6, "_c")
      )
      
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
      # r_covs <- r_selected$covariate
      # if (outcome == "arguments") {
      #   r_covs <- NULL
      # }
      z_covs <- z_selected$covariate
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
    paste0("(", 1:4, ")"),
    paste0("(", 1:2, ")"),
    paste0("(", 1:4, ")"),
    paste0("(", 1:4, ")"),
    paste0("(", 1:4, ")"),
    paste0("(", 1:4, ")")
    )

make_report_table(
  models = itt_secondary[1:4],
  outcomes = secondary_outcomes[1:2],
  outcome_labels = secondary_labels[1:2],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on self-reported relationship satisfaction among men and women \\label{tab:itt_satisfaction}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_satisfaction.tex"
  )

make_report_table(
  models = itt_secondary[5:8],
  outcomes = secondary_outcomes[3:4],
  outcome_labels = secondary_labels[3:4],
  title = "ITT estimates of effects of HEP on perception of partner's satisfaction with relationship among men and women \\label{tab:itt_perc_satisfaction}",
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
  title = "ITT estimates of effects of HEP on conflict outcomes \\label{tab:itt_conflict}",
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
  title = "ITT estimates of effects of HEP on attitudes \\label{tab:itt_attitudes}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_attitudes.tex"
  )

make_report_table(
  models = itt_secondary[17:20],
  outcomes = secondary_outcomes[9:10],
  outcome_labels = secondary_labels[9:10],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on alcohol consumption and mental health \\label{tab:itt_depression}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_depression.tex"
  )

make_report_table(
  models = itt_secondary[21:22],
  outcomes = secondary_outcomes[11],
  outcome_labels = secondary_labels[11],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on control ladder \\label{tab:itt_ladder}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_ladder.tex"
  )

make_report_table(
  models = itt_secondary[c(23:24, 29:30)],
  outcomes = secondary_outcomes[c(12,15)],
  outcome_labels = secondary_labels[c(12, 15)],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on alternative communcation indices \\label{tab:itt_alt_comm}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_alt_comm.tex"
  )


make_report_table(
  models = itt_secondary[25:28],
  outcomes = secondary_outcomes[13:14],
  outcome_labels = secondary_labels[13:14],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on communication questions only \\label{tab:itt_comm}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_comm.tex"
  )


make_report_table(
  models = itt_secondary[31:34],
  outcomes = secondary_outcomes[16:17],
  outcome_labels = secondary_labels[16:17],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on arguments about sex \\label{tab:itt_arg_sex}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_arg_sex.tex"
  )

make_report_table(
  models = itt_secondary[35:38],
  outcomes = secondary_outcomes[18:19],
  outcome_labels = secondary_labels[18:19],
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on attitudes about sex \\label{tab:itt_sex_attitudes}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_sex_attitudes.tex"
  )





