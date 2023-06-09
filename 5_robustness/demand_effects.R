itt_demand <- 
  map2(
    rep(demand_outcomes, each = 2),
    rep(c(FALSE, TRUE), length(demand_outcomes)),
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

names(itt_demand) <- 
  c(paste0("(", 1:4, ")"), 
    paste0("(", 1:4, ")"))

make_report_table(
  models = itt_demand[1:4],
  outcomes = demand_outcomes[1:2],
  outcome_labels = demand_labels[1:2],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_demand_charity.tex"
  )

make_report_table(
  models = itt_demand[5:8],
  outcomes = demand_outcomes[3:4],
  outcome_labels = demand_labels[3:4],
  treatment = "treatment",
  data = rmc
) |>
  save_kable(
    file = "6_tables/itt_demand_sdb.tex"
  )
