
# pre-specified -----------------------------------------------------------

subgroups <- c(
  "I(strata_new %in% c(2, 3, 5, 6))"
  # "sample_pi_m_bl",
  # "alcohol_man_w_bl",
  # "whatsapp_work_m_bl",
  # "batch",
  # "education_m_bl_8",
  # "reaction2_w_bl"
)

covs <- list(
  c("I(strata_new == 7)")
)

subgroup_labels <- list(
  c("Baseline IPV", "No baseline IPV")
  # "sample_pi_m_bl",
  # "alcohol_man_w_bl",
  # "whatsapp_work_m_bl",
  # "batch",
  # "education_m_bl_8",
  # "reaction2_w_bl"
)

files <- c(
  "6_tables/by_baseline_ipv.tex"
)

subgroup_tables <- 
  map2(subgroups,
       covs,
       function(group, covs) {
         fits <- map(
           outcomes,
           function(x) {
             y_covs <- y_selected$covariate[y_selected$outcome == x]
             #r_covs <- r_selected$covariate[r_selected$outcome == outcome]
             z_covs <- z_selected$covariate[z_selected$outcome == x]
             covs <- unique(c(y_covs, z_covs, covs))
             
             lm_robust(
               formula = reformulate(
                 termlabels = c(
                   "treatment",
                   group,
                   covs,
                   paste0("treatment:", group),
                   paste0("treatment:", covs)
                 ),
                 response = x
               ),
               data = subset(rmc, id_status == 1)
             )
           })
         
         names(fits) <- outcomes
         
         tab <- bind_rows(lapply(fits, tidy))
         
         tab |>
           filter(term %in% c("treatment", paste0("treatment:", group, "TRUE"))) |>
           mutate(term = case_when(term == "treatment" ~ "group1", TRUE ~ "group2")) |>
           select(term, outcome, estimate, p.value) |>
           pivot_wider(names_from = term, values_from = c(estimate, p.value)) |>
           mutate(
             outcome = outcome_labels,
             p.value_diff = p.value_group2,
             estimate_diff = add_stars(estimate_group2, p.value_diff), 
             estimate_group2 = estimate_group1 + estimate_group2
           ) |>
           select(outcome,
                  estimate_group2,
                  estimate_group1,
                  estimate_diff,
                  p.value_diff)
         
       })


pmap(
  list(
    subgroup_tables,
    subgroup_labels,
    files
  ),
  function(tab, labels, file) {
    kable(
      x = tab,
      format = "latex",
      digits = 3,
      col.names = c(
        "Outcome",
        labels,
        "{Difference (1-2)}",
        "P-value"
      ),
      align = c("l", "c", "c", "d", "c"),
      booktabs = TRUE,
      linesep = '',
      escape = FALSE
    ) |>
      kable_styling(latex_options = "hold_position") |>
      group_rows("Violence outcomes", 1, 12) |>
      group_rows("Other primary outcomes", 13, 15) |>
      group_rows("Secondary outcomes", 16, 26) |>
      group_rows("Experimenter demand outcomes", 27, 30) |>
      add_header_above(c(" " = 1, "(1)" = 1, "(2)" = 1, " " = 1, " " = 1)) |>
      save_kable(
        file = file
      )
  }
)



map2(
  subgroups,
  splits,
  function(x, y) {
    strata_FE <- paste0("strata_new_", 2:7, "_c")
    
    if (is.na(y)) {
      term <- c(paste0("factor(", x, ")"), strata_FE)
    } else {
      term <- c(paste0("I(", x, " > ", y, ")"), strata_FE)
    }
    lm_robust(
      reformulate(
        termlabels = c("treatment", term, paste0("treatment:", term)),
        response = "any_ipv",
      ),
      #fixed_effects = ~factor(strata),
      data = filter(rmc, id_status_w == 1)
    )      
  }
)


# data-driven -------------------------------------------------------------

get_cate <- function (outcome, covariates, treatment = "treatment", data, ...) {
  cc <- which(!is.na(data[[outcome]]))
  
  W <- data[[treatment]][cc]
  Y <- data[[outcome]][cc]
  X <- data[cc, covariates]

  tau <- causal_forest(X, Y, W, ...)
  
  return(tau)
}


cfs <- map(
  violence_outcomes,
  function(outcome) {
    strata_FE <- paste0("strata_new_", 2:7, "_c")
    y_covs <- y_selected$covariate[y_selected$outcome == outcome]
    y_covs <- c(y_covs, strata_FE)
    if (!length(y_covs) == 0) {
      cf <- get_cate(
        outcome = outcome,
        covariates = y_covs,
        data = subset(rmc, id_status_w == 1)
      )
    } else {
      NULL
    }
  }
)


cc <- which(!is.na(rmc[["any_ipv"]]))
train <- sample(cc, length(cc) / 2)
test <- cc[!cc %in% train]
X <- rmc[, c(
  bl_covariates,# y_selected$covariate[y_selected$outcome == "any_ipv"], 
  paste0("strata_new_", 2:7)
)]

forest.Y <- regression_forest(
  X = X[train, ], 
  Y = rmc[["any_ipv"]][train], 
  tune.parameters = "all"
  )

forest.Y.varimp <- variable_importance(forest.Y)
selected.vars <- which(forest.Y.varimp / mean(forest.Y.varimp) > 0.75)

cf <- causal_forest(
  X = X[cc, selected.vars],
  Y = rmc[["any_ipv"]][cc],
  W = rmc[["treatment"]][cc],
  W.hat = 0.5,
  tune.parameters = "all"
)

test_calibration(cf)
priority.cate <- -1 * predict(cf, X[test, selected.vars])$predictions

cf.eval <- causal_forest(
  X = X[test, selected.vars],
  Y = rmc[["any_ipv"]][test],
  W = rmc[["treatment"]][test],
  tune.parameters = "all"
)

rate <- rank_average_treatment_effect(cf.eval, priority.cate)

plot(rate)
