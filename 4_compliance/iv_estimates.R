

# estimate ITT effects ----------------------------------------------------
rmc <- 
  rmc |>
  mutate(
    remained_in_chat = replace(remained_in_chat, treatment == 0, 0)
  )

compliance_violence <- 
  map2(
    rep(violence_outcomes, each = 2),
    rep(c(FALSE, TRUE), length(violence_outcomes)),
    function(outcome, adjusted) {
      strata_FE <- c(
        paste0("strata_new_", 2:3, "_c")
        # paste0("batch_", 2:5, "_c")
      )
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
      r_covs <- r_selected$covariate
      z_covs <- z_selected$covariate
      covs <- unique(c(y_covs, r_covs, z_covs, strata_FE))
      len <- length(covs)
      lfe <- length(strata_FE)
      lm_robust(
        reformulate(
          termlabels = if (adjusted) {
            c(
              "remained_in_chat",
              covs,
              paste0("remained_in_chat:", covs[-len]),
              paste0(paste0("remained_in_chat:", covs[len]), " | treatment"),
              covs,
              paste0("treatment:", covs)
            )
          } else {
            c(
              "remained_in_chat",
              strata_FE,
              paste0("remained_in_chat:", strata_FE[-lfe]),
              paste0(paste0("remained_in_chat:", strata_FE[lfe]), " | treatment"),
              strata_FE,
              paste0("treatment:", strata_FE)
            )
          },
          response = outcome,
        ),
        data = filter(rmc, id_status_w == 1 & strata_new != 4)
        #weights = response_weights[[outcome]]
      )      
    }
  )

# make tables -------------------------------------------------------------

names(compliance_violence) <- 
  c(paste0("(", 1:6, ")"), 
    paste0("(", 1:6, ")"), 
    paste0("(", 1:6, ")"),
    paste0("(", 1:6, ")"))

make_report_table(
  models = compliance_violence[1:6],
  outcomes = violence_outcomes[1:3],
  outcome_labels = violence_labels[1:3],
  treatment = "remained_in_chat",
  title = "IV estimates of effects of remaining in WhatsApp chat on primary violence outcomes \\label{tab:itt_violence_bin_1}",
  data = rmc
) |>
  save_kable(
    file = "6_tables/iv_violence_bin_1.tex"
  )

