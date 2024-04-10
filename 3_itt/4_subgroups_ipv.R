# analyze outcomes by baseline violence -----------------------------------

subgroups <- c(
  "I(strata_new %in% c(2, 3))"
)

covs <- list(
  c("I(strata_new == 4)")
)

subgroup_labels <- list(
  c("{Baseline IPV}", "{No baseline IPV}")
)

files <- c(
  "6_tables/by_baseline_ipv.tex"
)

subgroup_tables <- 
  map2(subgroups,
       covs,
       function(group, covs) {
         fits <- map(
           c(outcomes, paste0("I(", violence_items, ">1)")),
           function(x) {
             lm_robust(
               formula = reformulate(
                 termlabels = c(
                   "treatment",
                   group,
                   c(covs, paste0("batch_", 2:5, "_c")),
                   paste0("treatment:", group),
                   paste0("treatment:", c(covs, paste0("batch_", 2:5, "_c")))
                 ),
                 response = x
               ),
               data = subset(rmc, id_status_w == 1)
             )
           })
         
         fits1 <- map(
           c(outcomes, paste0("I(", violence_items, ">1)")),
           function(x) {
             lm_robust(
               formula = reformulate(
                 termlabels = c(
                   "treatment",
                   c(paste0("batch_", 2:5, "_c_s2"), "strata_new_3_c_s2"),
                   paste0("treatment:", c(paste0("batch_", 2:5, "_c_s2"), "strata_new_3_c_s2"))
                 ),
                 response = x
               ),
               data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
             )
           })
         
         fits0 <- map(
           c(outcomes, paste0("I(", violence_items, ">1)")),
           function(x) {
             lm_robust(
               formula = reformulate(
                 termlabels = c(
                   "treatment",
                   c(paste0("batch_", 2:5, "_c_s1")),
                   paste0("treatment:", c(paste0("batch_", 2:5, "_c_s1")))
                 ),
                 response = x
               ),
               data = subset(rmc, id_status_w == 1 & strata_new %in% c(1))
             )
           })
         
         pvalues1 <- sapply(
           fits1, 
           function(x) pull(filter(tidy(x), term == "treatment"), "p.value")
          )
         
         pvalues0 <- sapply(
           fits0, 
           function(x) pull(filter(tidy(x), term == "treatment"), "p.value")
         )
         
         est1 <- sapply(
           fits1, 
           function(x) pull(filter(tidy(x), term == "treatment"), "estimate")
         )
         
         est0 <- sapply(
           fits0, 
           function(x) pull(filter(tidy(x), term == "treatment"), "estimate")
         )
         
         names(fits) <- c(outcomes, paste0("I(", violence_items, ">1)"))

         tab <- bind_rows(lapply(fits, tidy))

         tab |>
           filter(term %in% c(
             "(Intercept)",
             "treatment",
             paste0(group, "TRUE"),
             paste0("treatment:", group, "TRUE")
           )) |>
           mutate(term = case_when(
             term == "treatment" ~ "group2",
             term == "(Intercept)" ~ "int2",
             term == paste0(group, "TRUE") ~ "int1",
             TRUE ~ "group1"
           )) |>
           select(term, outcome, estimate, p.value) |>
           pivot_wider(names_from = term, values_from = c(estimate, p.value)) |>
           mutate(
             outcome = c(outcome_labels, violence_item_descriptions),
             p.value_diff = p.value_group1,
             estimate_diff = add_stars(estimate_group1, p.value_diff), 
             mean_group1 = estimate_int1 + estimate_int2,
             mean_group2 = estimate_int2,
             estimate_group1 = add_stars(est1, pvalues1),
             estimate_group2 = add_stars(est0, pvalues0)
           ) |>
           select(outcome,
                  mean_group1,
                  estimate_group1,
                  mean_group2,
                  estimate_group2,
                  estimate_diff,
                  p.value_diff)
         
       })


subgroup_tables_w_covs <- 
  map(list(c(2, 3), c(1)),
      function(s) {
        fits <- map(
          c(outcomes, paste0("I(", violence_items, ">1)")),
          function(x) {
            y_covs <- y_selected_strata$covariate[
              y_selected_strata$outcome == x &
                y_selected_strata$strata == s[1]
            ]
            # r_covs <- r_selected_strata$covariate[
            #   r_selected_strata$strata == s[1]
            # ]
            z_covs <- z_selected_strata$covariate[
                z_selected_strata$strata == s[1]
            ]
            if (s[1] == 2) {
              covs <-
                unique(c(
                  y_covs,
                  z_covs,
                  paste0("batch_", 2:5, "_c_s", s[1]),
                  paste0("strata_new_", 3, "_c_s", s[1])
                ))
            } else {
              covs <-
                unique(c(
                  y_covs,
                  z_covs,
                  paste0("batch_", 2:5, "_c_s", s[1])
                ))
            }
            if (length(covs) > 0) {
              f <- lm_robust(
                formula = reformulate(
                  termlabels = c(
                    "treatment",
                    covs,
                    paste0("treatment:", covs)
                  ),
                  response = x
                ),
                data = subset(rmc, id_status_w == 1 & strata_new %in% s)
              ) 
              if (x == "any_physical") {
                lm_robust(
                  formula = reformulate(
                    termlabels = c(
                      "treatment",
                      covs[!covs %in% c("ipv11_w_bl_4_c_s2")],
                      paste0("treatment:", covs)
                    ),
                    response = x
                  ),
                  data = subset(rmc, id_status_w == 1 & strata_new %in% s)
                ) |> print()
              }
              f
            } else{
              lm_robust(
                formula = reformulate(
                  termlabels = c(
                    "treatment"
                  ),
                  response = x
                ),
                data = subset(rmc, id_status_w == 1 & strata_new %in% s)
              )
            }
            
          })
        names(fits) <- c(outcomes, paste0("I(", violence_items, ">1)"))
        
        
        tab <- bind_rows(lapply(fits, tidy))
        nobs <- sapply(fits, function(x) glance(x)$nobs[1])
        tab |>
          filter(term %in% c("treatment", "(Intercept)")) |>
          select(term, outcome, estimate, std.error, p.value) |>
          mutate(
            strata = if_else(s[1] == 2, "group1", "group2"), 
            term = if_else(term == "treatment", "", "mean"),
            nobs = rep(nobs, each = 2))
      })

subgroup_tables_w_covs <- 
  bind_rows(subgroup_tables_w_covs) |>
  pivot_wider(
    names_from = c(term, strata), 
    values_from = c(nobs, estimate, std.error, p.value)
  ) |>
  rename(
    mean_group1 = estimate_mean_group1,
    mean_group2 = estimate_mean_group2
  ) |>
  rename_with(
    ~str_replace(.x, "__", "_")
  ) |>
  select(-matches("_mean_")) |>
  mutate(
    outcome = c(outcome_labels, violence_item_descriptions),
    p.value_diff = pnorm(
      -abs(estimate_group1 - estimate_group2) / 
        sqrt(std.error_group1^2 + std.error_group2^2)) * 2,
    estimate_diff = add_stars(estimate_group1 - estimate_group2, p.value_diff), 
    estimate_group2 = add_stars(estimate_group2, p.value_group2),
    estimate_group1 = add_stars(estimate_group1, p.value_group1)
  ) |>
  select(outcome,
         nobs_group1,
         mean_group1,
         estimate_group1,
         nobs_group2,
         mean_group2,
         estimate_group2,
         estimate_diff,
         p.value_diff) 

kable(
  x = left_join(
    subgroup_tables[[1]],
    select(subgroup_tables_w_covs, outcome, nobs_group1, nobs_group2),
    by = "outcome"
  ) |>
    select(outcome,
           nobs_group1,
           mean_group1,
           estimate_group1,
           nobs_group2,
           mean_group2,
           estimate_group2,
           estimate_diff,
           p.value_diff) |>
    mutate(
      outcome = str_replace(outcome, "&", "\\\\&"),
      nobs_group1 = as.numeric(nobs_group1),
      nobs_group2 = as.numeric(nobs_group2)
    ) |>
    filter(outcome %in% violence_labels_no_stack),
  format = "latex",
  digits = 3,
  col.names = c(
    "Outcome",
    "{(1)}",
    "{(2)}",
    "{(3)}",
    "{(4)}",
    "{(5)}",
    "{(6)}",
    "{(3) - (6)}",
    "{(7)}"
  ),
  align = c("l", "D", "d", "d", "D", "d", "d", "d", "c"),
  booktabs = TRUE,
  caption = "Effect of HEP among couples reporting physical or sexual IPV at baseline compared to those reporting no physical or sexual IPV violence at baseline NO COVARIATES\\label{tab:hetfx1a}",
  linesep = '',
  escape = FALSE,
  format.args = list(big.mark = ",")
) |>
  kable_styling(font_size = 9, 
                latex_options = "hold_position") |>
  add_header_above(c(" "= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "Difference"= 1,
                     "P-value" = 1), line = FALSE) |>
  add_header_above(c(" " = 1, "Subgroup: IPV" = 3, "Subgroup: No IPV" = 3, " " = 2)) |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) |>
  save_kable(
    file = "6_tables/by_baseline_ipv_1a.tex"
  )

kable(
  x = subgroup_tables_w_covs |>
    select(outcome,
           nobs_group1,
           mean_group1,
           estimate_group1,
           nobs_group2,
           mean_group2,
           estimate_group2,
           estimate_diff,
           p.value_diff) |>
    mutate(
      outcome = str_replace(outcome, "&", "\\\\&"),
      nobs_group1 = as.numeric(nobs_group1),
      nobs_group2 = as.numeric(nobs_group2)
    ) |>
    filter(outcome %in% violence_labels_no_stack),
  format = "latex",
  digits = 3,
  col.names = c(
    "Outcome",
    "{(1)}",
    "{(2)}",
    "{(3)}",
    "{(4)}",
    "{(5)}",
    "{(6)}",
    "{(3) - (6)}",
    "{(7)}"
  ),
  align = c("l", "D", "d", "d", "D", "d", "d", "d", "c"),
  booktabs = TRUE,
  caption = "Effect of HEP among couples reporting physical or sexual IPV at baseline compared to those reporting no physical or sexual IPV violence at baseline WITH COVARIATES\\label{tab:hetfx1b}",
  linesep = '',
  escape = FALSE,
  format.args = list(big.mark = ",")
) |>
  kable_styling(font_size = 9, 
                latex_options = "hold_position") |>
  add_header_above(c(" "= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "Difference"= 1,
                     "P-value" = 1), line = FALSE) |>
  add_header_above(c(" " = 1, "Subgroup: IPV" = 3, "Subgroup: No IPV" = 3, " " = 2)) |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) |>
  save_kable(
    file = "6_tables/by_baseline_ipv_1.tex"
  )


kable(
  x = left_join(
    subgroup_tables[[1]],
    select(subgroup_tables_w_covs, outcome, nobs_group1, nobs_group2),
    by = "outcome"
  ) |>
    select(outcome,
           nobs_group1,
           mean_group1,
           estimate_group1,
           nobs_group2,
           mean_group2,
           estimate_group2,
           estimate_diff,
           p.value_diff) |>
    mutate(
      outcome = str_replace(outcome, "&", "\\\\&"),
      nobs_group1 = as.numeric(nobs_group1),
      nobs_group2 = as.numeric(nobs_group2)
    ) |>
    filter(!outcome %in% c(violence_labels_no_stack, violence_item_descriptions)),
  format = "latex",
  digits = 3,
  col.names = c(
    "Outcome",
    "{(1)}",
    "{(2)}",
    "{(3)}",
    "{(4)}",
    "{(5)}",
    "{(6)}",
    "{(3) - (6)}",
    "{(7)}"
  ),
  align = c("l", "D", "d", "d", "D", "d", "d", "d", "c"),
  booktabs = TRUE,
  caption = "Effect of HEP among couples reporting physical or sexual IPV at baseline compared to those reporting no physical or sexual IPV violence at baseline NO COVARIATES \\label{tab:hetfx2a}",
  linesep = '',
  escape = FALSE,
  format.args = list(big.mark = ",")
) |>
  kable_styling(font_size = 9, 
                latex_options = "hold_position") |>
  group_rows("Other primary outcomes", 1, 3, italic = T, bold = F) |>
  group_rows("Secondary outcomes", 4, 22, italic = T, bold = F) |>
  group_rows("Experimenter demand outcomes", 23, 26, italic = T, bold = F) |>
  #row_spec()
  add_header_above(c(" "= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "Difference"= 1,
                     "P-value" = 1), line = FALSE) |>
  add_header_above(c(" " = 1, "Subgroup: IPV" = 3, "Subgroup: No IPV" = 3, " " = 2)) |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) |>
  save_kable(
    file = "6_tables/by_baseline_ipv_2a.tex"
  )


kable(
  x = subgroup_tables_w_covs |>
    select(outcome,
           nobs_group1,
           mean_group1,
           estimate_group1,
           nobs_group2,
           mean_group2,
           estimate_group2,
           estimate_diff,
           p.value_diff) |>
    mutate(
      outcome = str_replace(outcome, "&", "\\\\&"),
      nobs_group1 = as.numeric(nobs_group1),
      nobs_group2 = as.numeric(nobs_group2)
    ) |>
    filter(!outcome %in% c(violence_labels_no_stack, violence_item_descriptions)),
  format = "latex",
  digits = 3,
  col.names = c(
    "Outcome",
    "{(1)}",
    "{(2)}",
    "{(3)}",
    "{(4)}",
    "{(5)}",
    "{(6)}",
    "{(3) - (6)}",
    "{(7)}"
  ),
  align = c("l", "D", "d", "d", "D", "d", "d", "d", "c"),
  booktabs = TRUE,
  caption = "Effect of HEP among couples reporting physical or sexual IPV at baseline compared to those reporting no physical or sexual IPV violence at baseline WITH COVARIATES \\label{tab:hetfx2b}",
  linesep = '',
  escape = FALSE,
  format.args = list(big.mark = ",")
) |>
  kable_styling(font_size = 9, 
                latex_options = "hold_position") |>
  group_rows("Other primary outcomes", 1, 3, italic = T, bold = F) |>
  group_rows("Secondary outcomes", 4, 22, italic = T, bold = F) |>
  group_rows("Experimenter demand outcomes", 23, 26, italic = T, bold = F) |>
  #row_spec()
  add_header_above(c(" "= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "Difference"= 1,
                     "P-value" = 1), line = FALSE) |>
  add_header_above(c(" " = 1, "Subgroup: IPV" = 3, "Subgroup: No IPV" = 3, " " = 2)) |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) |>
  save_kable(
    file = "6_tables/by_baseline_ipv_2.tex"
  )

kable(
  x = left_join(
    subgroup_tables[[1]],
    select(subgroup_tables_w_covs, outcome, nobs_group1, nobs_group2),
    by = "outcome"
  ) |>
    select(outcome,
           nobs_group1,
           mean_group1,
           estimate_group1,
           nobs_group2,
           mean_group2,
           estimate_group2,
           estimate_diff,
           p.value_diff) |>
    mutate(
      outcome = str_replace(outcome, "&", "\\\\&"),
      nobs_group1 = as.numeric(nobs_group1),
      nobs_group2 = as.numeric(nobs_group2)
    ) |>
    filter(outcome %in% violence_item_descriptions),
  format = "latex",
  digits = 3,
  col.names = c(
    "Outcome",
    "{(1)}",
    "{(2)}",
    "{(3)}",
    "{(4)}",
    "{(5)}",
    "{(6)}",
    "{(3) - (6)}",
    "{(7)}"
  ),
  align = c("l", "D", "d", "d", "D", "d", "d", "d", "c"),
  booktabs = TRUE,
  caption = "Effect of HEP among couples reporting physical or sexual IPV at baseline compared to those reporting no physical or sexual IPV violence at baseline NO COVARIATES \\label{tab:hetfx3a}",
  linesep = '',
  escape = FALSE,
  format.args = list(big.mark = ",")
) |>
  kable_styling(font_size = 9, 
                latex_options = "hold_position") |>
  add_header_above(c(" "= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "Difference"= 1,
                     "P-value" = 1), line = FALSE) |>
  add_header_above(c(" " = 1, "Subgroup: IPV" = 3, "Subgroup: No IPV" = 3, " " = 2)) |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) |>
  save_kable(
    file = "6_tables/by_baseline_ipv_3a.tex"
  )

kable(
  x = subgroup_tables_w_covs |>
    select(outcome,
           nobs_group1,
           mean_group1,
           estimate_group1,
           nobs_group2,
           mean_group2,
           estimate_group2,
           estimate_diff,
           p.value_diff) |>
    mutate(
      outcome = str_replace(outcome, "&", "\\\\&"),
      nobs_group1 = as.numeric(nobs_group1),
      nobs_group2 = as.numeric(nobs_group2)
    ) |>
    filter(outcome %in% violence_item_descriptions),
  format = "latex",
  digits = 3,
  col.names = c(
    "Outcome",
    "{(1)}",
    "{(2)}",
    "{(3)}",
    "{(4)}",
    "{(5)}",
    "{(6)}",
    "{(3) - (6)}",
    "{(7)}"
  ),
  align = c("l", "D", "d", "d", "D", "d", "d", "d", "c"),
  booktabs = TRUE,
  caption = "Effect of HEP among couples reporting physical or sexual IPV at baseline compared to those reporting no physical or sexual IPV violence at baseline WITH COVARIATES\\label{tab:hetfx3b}",
  linesep = '',
  escape = FALSE,
  format.args = list(big.mark = ",")
) |>
  kable_styling(font_size = 9, 
                latex_options = "hold_position") |>
  add_header_above(c(" "= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "N"= 1,
                     "Mean"= 1,
                     "Estimate"= 1,
                     "Difference"= 1,
                     "P-value" = 1), line = FALSE) |>
  add_header_above(c(" " = 1, "Subgroup: IPV" = 3, "Subgroup: No IPV" = 3, " " = 2)) |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) |>
  save_kable(
    file = "6_tables/by_baseline_ipv_3.tex"
  )


# expanded definition -----------------------------------------------------

rmc <- 
  rmc |>
  mutate(
    subgroup_physex_bl = ifelse(strata_new %in% c(2, 3), 1, 0),
    subgroup_psych_bl = ifelse(strata_new == 1 & p_violence_bl > 0.2, 1, 0),
    subgroup_none_bl = ifelse(strata_new == 1 & p_violence_bl <= 0.2, 1, 0),
    expanded_subgroup = case_when(
      subgroup_physex_bl == 1 ~ "IPV",
      subgroup_psych_bl == 1 ~ "No IPV: high propensity",
      subgroup_none_bl == 1 ~ "No IPV: low propensity",
      TRUE ~ NA_character_
    )
  )

subgroup_psych_fits_diff <- map(
  outcomes,
  function(x) {
    lm_robust(
      formula = reformulate(
        termlabels = c(
          "treatment",
          "subgroup_physex_bl",
          "subgroup_psych_bl",
          "treatment:subgroup_physex_bl",
          "treatment:subgroup_psych_bl"
        ),
        response = x
      ),
      data = subset(rmc, id_status_w == 1 & strata_new %in% c(1, 2, 3))
    )
  })

subgroup_psych_fits_no_prods <- map(
  outcomes,
  function(x) {
    lm_robust(
      formula = reformulate(
        termlabels = c(
          "treatment",
          "subgroup_physex_bl",
          "subgroup_psych_bl"
        ),
        response = x
      ),
      data = subset(rmc, id_status_w == 1 & strata_new %in% c(1, 2, 3))
    )
  })

pvalues_diff <- map2_dbl(
  subgroup_psych_fits_diff, 
  subgroup_psych_fits_no_prods,
  function(x, y) lmtest::waldtest(x,y)$`Pr(>Chisq)`[2]
)

subgroup_psych_fits_none <- map(
  outcomes,
  function(x) {
    lm_robust(
      formula = reformulate(
        termlabels = "treatment",
        response = x
      ),
      data = subset(rmc, id_status_w == 1 & strata_new == 1 & subgroup_none_bl == 1)
    )
  })

nobs_none <- sapply(subgroup_psych_fits_none, function(x) glance(x)$nobs[1])

subgroup_psych_fits_physex <- map(
  outcomes,
  function(x) {
    lm_robust(
      formula = reformulate(
        termlabels = "treatment",
        response = x
      ),
      data = subset(rmc, id_status_w == 1 & strata_new %in% c(1, 2, 3) & subgroup_physex_bl == 1)
    )
  })

nobs_physex <- sapply(subgroup_psych_fits_physex, function(x) glance(x)$nobs[1])

subgroup_psych_fits_psych <- map(
  outcomes,
  function(x) {
    lm_robust(
      formula = reformulate(
        termlabels = "treatment",
        response = x
      ),
      data = subset(rmc, id_status_w == 1 & strata_new %in% c(1, 2, 3) & subgroup_psych_bl == 1)
    )
  })

nobs_psych <- sapply(subgroup_psych_fits_psych, function(x) glance(x)$nobs[1])

pvalues_physex <- sapply(
  subgroup_psych_fits_physex, 
  function(x) pull(filter(tidy(x), term == "treatment"), "p.value")
)

pvalues_psych <- sapply(
  subgroup_psych_fits_psych, 
  function(x) pull(filter(tidy(x), term == "treatment"), "p.value")
)

names(subgroup_psych_fits_diff) <- outcomes

tab <- bind_rows(lapply(subgroup_psych_fits_diff, tidy))

tab <-
  tab |>
  filter(str_detect(term, 'treatment')) |>
  mutate(
    term = case_when(
      term == "treatment" ~ "group3", 
      str_detect(term, 'subgroup_psych_bl') ~ "group2",
      str_detect(term, 'subgroup_physex_bl') ~ "group1"
    )
  ) |>
  select(term, outcome, estimate, p.value) |>
  pivot_wider(names_from = term, values_from = c(estimate, p.value)) |>
  mutate(
    outcome = outcome_labels,
    outcome = str_replace(outcome, "&", "\\\\&"),
    p.value_diff = pvalues_diff,
    nobs_group1 = as.numeric(nobs_physex),
    nobs_group2 = as.numeric(nobs_psych),
    nobs_group3 = as.numeric(nobs_none),
    estimate_group1 = add_stars(estimate_group3 + estimate_group1, pvalues_physex),
    estimate_group2 = add_stars(estimate_group3 + estimate_group2, pvalues_psych),
    estimate_group3 = add_stars(estimate_group3, p.value_group3)
  ) |>
  select(outcome,
         nobs_group1,
         estimate_group1,
         nobs_group2,
         estimate_group2,
         nobs_group3,
         estimate_group3,
         p.value_diff)

kable(
  x = filter(tab, outcome %in% violence_labels_no_stack),
  format = "latex",
  digits = 3,
  col.names = c(
    "Outcome",
    "{N}",
    "{Estimate}",
    "{N}",
    "{Estimate}",
    "{N}",
    "{Estimate}",
    "P-value"
  ),
  caption = "Effect of HEP among couples reporting physical or sexual IPV at baseline compared to those reporting no physical or sexual IPV violence at baseline after splitting those who did not report violence into low and high propensity couples \\label{tab:hetfx_propensity1}",
  align = c("l", "D", "d", "D", "d", "D", "d", "c"),
  booktabs = TRUE,
  linesep = '',
  escape = FALSE
) |>
  kable_styling(latex_options = "hold_position") |>
  add_header_above(c(" "= 1,
                     "IPV"= 2,
                     "No IPV:\nhigh propensity"= 2,
                     "No IPV:\nlow propensity"= 2,
                     " "= 1)) |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) |>
  save_kable(
    file = "6_tables/by_baseline_propensity_1.tex"
  )

kable(
  x = filter(tab, !outcome %in% violence_labels_no_stack),
  format = "latex",
  digits = 3,
  col.names = c(
    "Outcome",
    "{N}",
    "{Estimate}",
    "{N}",
    "{Estimate}",
    "{N}",
    "{Estimate}",
    "P-value"
  ),
  caption = "Effect of HEP among couples reporting physical or sexual IPV at baseline compared to those reporting no physical or sexual IPV violence at baseline after splitting those who did not report violence into low and high propensity couples \\label{tab:hetfx_propensity2}",
  align = c("l", "D", "d", "D", "d", "D", "d", "c"),
  booktabs = TRUE,
  linesep = '',
  escape = FALSE
) |>
  kable_styling(latex_options = "hold_position") |>
  group_rows("Other primary outcomes", 1, 3, italic = T, bold = F) |>
  group_rows("Secondary outcomes", 4, 22, italic = T, bold = F) |>
  group_rows("Experimenter demand outcomes", 23, 26, italic = T, bold = F) |>
  add_header_above(c(" "= 1,
                     "IPV"= 2,
                     "No IPV:\nhigh propensity"= 2,
                     "No IPV:\nlow propensity"= 2,
                     " "= 1)) |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) |>
  save_kable(
    file = "6_tables/by_baseline_propensity_2.tex"
  )

         

itt_refusals <- 
  map2(
    rep(c("any_ipv_refusals"), each = 2),
    rep(c(FALSE, TRUE), 1),
    function(outcome, adjusted) {
      y_covs <- y_selected_strata$covariate[
        y_selected_strata$outcome == outcome &
          y_selected_strata$strata == 1
      ]
      # r_covs <- r_selected_strata$covariate[
      #   r_selected_strata$strata == 1
      # ]
      z_covs <- z_selected_strata$covariate[
        z_selected_strata$strata == 1
      ]
      covs <- unique(c(y_covs, z_covs))
      
      lm_robust(
        reformulate(
          termlabels = if (adjusted & length(covs) > 0) {
            c("treatment", covs, paste0("treatment:", covs))
          } else {
            c("treatment")
          },
          response = outcome,
        ),
        data = filter(rmc, id_status_w == 1 & strata_new == 1)
      )      
    }
  )

itt_refusals_bl <- 
  map2(
    rep(c("any_ipv_refusals_bl"), each = 2),
    rep(c(FALSE, TRUE), 1),
    function(outcome, adjusted) {
      y_covs <- y_selected_strata_bl$covariate
      
      covs <- unique(c(y_covs))
      
      lm_robust(
        reformulate(
          termlabels = if (adjusted & length(covs) > 0) {
            c("treatment", covs, paste0("treatment:", covs))
          } else {
            c("treatment")
          },
          response = outcome,
        ),
        data = subset(rmc, strata_new == 1),
      )      
    }
  )

map2(
  rep(c("any_ipv_refusals"), each = 2),
  rep(c(FALSE, TRUE), 1),
  function(outcome, adjusted) {
    y_covs <- y_selected_strata$covariate[
      y_selected_strata$outcome == outcome &
        y_selected_strata$strata == 1
    ]
    # r_covs <- r_selected_strata$covariate[
    #   r_selected_strata$strata == 1
    # ]
    z_covs <- z_selected_strata$covariate[
      z_selected_strata$strata == 1
    ]
    covs <- unique(c(y_covs, z_covs))
    
    lm_robust(
      reformulate(
        termlabels = if (adjusted & length(covs) > 0) {
          c("treatment", covs, paste0("treatment:", covs), "subgroup_psych_bl", "treatment:subgroup_psych_bl")
        } else {
          c("treatment", "subgroup_psych_bl", "treatment:subgroup_psych_bl")
        },
        response = outcome,
      ),
      data = filter(rmc, id_status_w == 1 & strata_new == 1)
    )      
  }
)

rmc$any_ipv_impute_refusals <- replace(rmc$any_ipv, is.na(rmc$any_ipv), 1)

lm_robust(
  any_ipv_impute_refusals ~ treatment,
  data = subset(rmc, id_status_w == 1 & subgroup_psych_bl == 1)
)

map2(
  rep(c("any_ipv_refusals_bl"), each = 2),
  rep(c(FALSE, TRUE), 1),
  function(outcome, adjusted) {
    y_covs <- y_selected_strata_bl$covariate
    
    covs <- unique(c(y_covs))
    
    lm_robust(
      reformulate(
        termlabels = if (adjusted & length(covs) > 0) {
          c("treatment", covs, paste0("treatment:", covs), "subgroup_psych_bl", "treatment:subgroup_psych_bl")
        } else {
          c("treatment", "subgroup_psych_bl", "treatment:subgroup_psych_bl")
        },
        response = outcome,
      ),
      data = subset(rmc, strata_new == 1),
    )      
  }
)

itt_refusals <- c(itt_refusals_bl, itt_refusals)

names(itt_refusals) <- 
  c(paste0("(", 1:4, ")"))

make_report_table(
  models = itt_refusals,
  outcomes = c("any_ipv_refusals_bl", "any_ipv_refusals"),
  outcome_labels = c("Baseline IPV refusals", "Endline IPV refusals"),
  treatment = "treatment",
  title = "ITT estimates of effects of HEP on refusal to answer violence questions among those who reported no violence at baseline. \\label{tab:itt_refusals}",
  data = subset(rmc, strata_new == 1 & id_status_w == 1)
) |>
  save_kable(
    file = "6_tables/itt_refusals.tex"
  )
