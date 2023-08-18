
# pre-specified -----------------------------------------------------------

subgroups <- c(
  "I(strata_new %in% c(2, 3))"
  # "sample_pi_m_bl",
  # "alcohol_man_w_bl",
  # "whatsapp_work_m_bl",
  # "batch",
  # "education_m_bl_8",
  # "reaction2_w_bl"
)

covs <- list(
  c("I(strata_new == 4)")
)

subgroup_labels <- list(
  c("{Baseline IPV}", "{No baseline IPV}")
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
               data = subset(rmc, id_status_w == 1)
             )
           })
         
         fits1 <- map(
           outcomes,
           function(x) {
             lm_robust(
               formula = reformulate(
                 termlabels = c("treatment"),
                 response = x
               ),
               data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
             )
           })
         
         
         pvalues1 <- sapply(
           fits1, 
           function(x) pull(filter(tidy(x), term == "treatment"), "p.value")
          )
         
         names(fits) <- outcomes
         
         tab <- bind_rows(lapply(fits, tidy))
         
         tab |>
           filter(term %in% c("treatment", paste0("treatment:", group, "TRUE"))) |>
           mutate(term = case_when(term == "treatment" ~ "group2", TRUE ~ "group1")) |>
           select(term, outcome, estimate, p.value) |>
           pivot_wider(names_from = term, values_from = c(estimate, p.value)) |>
           mutate(
             outcome = outcome_labels,
             p.value_diff = p.value_group1,
             estimate_diff = add_stars(estimate_group1, p.value_diff), 
             estimate_group1 = add_stars(estimate_group1 + estimate_group2, pvalues1),
             estimate_group2 = add_stars(estimate_group2, p.value_group2)
           ) |>
           select(outcome,
                  estimate_group1,
                  estimate_group2,
                  estimate_diff,
                  p.value_diff)
         
       })


# pmap(
#   list(
#     subgroup_tables,
#     subgroup_labels,
#     files
#   ),
#   function(tab, labels, file) {
#     kable(
#       x = tab,
#       format = "latex",
#       digits = 3,
#       col.names = c(
#         "Outcome",
#         labels,
#         "{Difference (1-2)}",
#         "P-value"
#       ),
#       align = c("l", "d", "d", "d", "c"),
#       booktabs = TRUE,
#       linesep = '',
#       escape = FALSE
#     ) |>
#       kable_styling(latex_options = "hold_position") |>
#       group_rows("Violence outcomes", 1, 12) |>
#       group_rows("Other primary outcomes", 13, 15) |>
#       group_rows("Secondary outcomes", 16, 26) |>
#       group_rows("Experimenter demand outcomes", 27, 30) |>
#       add_header_above(c(" " = 1, "(1)" = 1, "(2)" = 1, " " = 1, " " = 1)) |>
#       save_kable(
#         file = file
#       )
#   }
# )


subgroup_tables_w_covs <- 
  map(list(c(2,3), c(1)),
      function(s) {
        fits <- map(
          outcomes,
          function(x) {
            y_covs <- y_selected_strata$covariate[
              y_selected_strata$outcome == x &
                y_selected_strata$strata == s[1]
            ]
            r_covs <- r_selected_strata$covariate[
              r_selected_strata$strata == s[1]
            ]
            z_covs <- z_selected_strata$covariate[
                z_selected_strata$strata == s[1]
            ]
            # z_covs <- NULL
            covs <- unique(c(y_covs, r_covs, z_covs, paste0("batch_", 2:5, "_c_s", s[1])))
            # covs <- NULL
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
              
              # if (x %in% c("any_ipv", "any_physical", "any_sexual") & s[1] == 1) {
              #   print(summary(f))
              # }
              
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
        
        names(fits) <- outcomes
        tab <- bind_rows(lapply(fits, tidy))
        nobs <- sapply(fits, function(x) glance(x)$nobs[1])
        tab |>
          filter(term %in% c("treatment")) |>
          select(term, outcome, estimate, std.error, p.value) |>
          mutate(strata = if_else(s[1] == 2, "group1", "group2"), nobs = nobs)
      })

subgroup_tables_w_covs <- 
  bind_rows(subgroup_tables_w_covs) |>
  pivot_wider(
    names_from = strata, 
    values_from = c(nobs, estimate, std.error, p.value)
  ) |>
  mutate(
    outcome = outcome_labels,
    p.value_diff = pnorm(
      -abs(estimate_group1 - estimate_group2) / 
        sqrt(std.error_group1^2 + std.error_group2^2)) * 2,
    estimate_diff = add_stars(estimate_group1 - estimate_group2, p.value_diff), 
    estimate_group2 = add_stars(estimate_group2, p.value_group2),
    estimate_group1 = add_stars(estimate_group1, p.value_group1)
  ) |>
  select(outcome,
         nobs_group1,
         nobs_group2,
         estimate_group1,
         estimate_group2,
         estimate_diff,
         p.value_diff) 

# kable(
#   x = left_join(
#     subgroup_tables[[1]], 
#     subgroup_tables_w_covs, 
#     by = "outcome"
#   ) |> 
#     select(outcome,
#            nobs_group1,
#            matches("group1"),
#            nobs_group2,
#            matches("group2"),
#            estimate_diff.x,
#            p.value_diff.x,
#            estimate_diff.y,
#            p.value_diff.y),
#   #|> relocate(c(nobs_group1, nobs_group2), .after = outcome),
#   format = "latex",
#   digits = 3,
#   col.names = c(
#     "Outcome",
#     "N",
#     "{Unadjusted}",
#     "{Adjusted}",
#     "N",
#     "{Unadjusted}",
#     "{Adjusted}",
#     "{Difference}",
#     "P-value",
#     "{Difference}",
#     "P-value"
#   ),
#   align = c("l", "c", "d", "d", "c", "d", "d", "d", "c", "d", "c"),
#   booktabs = TRUE,
#   linesep = '',
#   escape = FALSE
# ) |>
# kable_styling(font_size = 9, latex_options = "hold_position") |>
# group_rows("Violence outcomes", 1, 12) |>
# group_rows("Other primary outcomes", 13, 15) |>
# group_rows("Secondary outcomes", 16, 26) |>
# group_rows("Experimenter demand outcomes", 27, 30) |>
# # add_header_above(c("Outcome",
# #                    c("{IPV}", "{No IPV}"),
# #                    "{Difference}",
# #                    "P-value",
# #                    c("{IPV}", "{No IPV}"),
# #                    "{Difference}",
# #                    "P-value")) |>
# add_header_above(c(" " = 1, "Baseline IPV" = 3, "No baseline IPV" = 3,  " " = 4)) |>
# save_kable(
#   file = "6_tables/by_baseline_ipv.tex"
# )


kable(
  x = left_join(
    subgroup_tables[[1]],
    subgroup_tables_w_covs,
    by = "outcome"
  ) |>
    relocate(c(nobs_group1, nobs_group2), .after = outcome) |>
    mutate(
      outcome = str_replace(outcome, "&", "\\\\&"),
      nobs_group1 = as.numeric(nobs_group1),
      nobs_group2 = as.numeric(nobs_group2)
    ) |>
    filter(!str_detect(outcome, "Z-score")),
  format = "latex",
  digits = 3,
  col.names = c(
    "Outcome",
    "{(1)}",
    "{(2)}",
    "{(1a)}",
    "{(2a)}",
    "{(1a)-(2a)}",
    "(3a)",
    "{(1b)}",
    "{(2b)}",
    "{(1b)-(2b)}",
    "(3b)"
  ),
  align = c("l", "D", "D", "d", "d", "d", "c", "d", "d", "d", "c"),
  booktabs = TRUE,
  caption = "Effect of HEP among couples reporting physical or sexual IPV at baseline compared to those reporting no physical or sexual IPV violence at baseline \\label{tab:hetfx}",
  linesep = '',
  escape = FALSE,
  format.args = list(big.mark = ",")
) |>
  kable_styling(font_size = 9, 
                latex_options = "hold_position") |>
  group_rows("Violence outcomes", 1, 15, italic = T, bold = F) |>
  group_rows("Other primary outcomes", 16, 18, italic = T, bold = F) |>
  group_rows("Secondary outcomes", 19, 29, italic = T, bold = F) |>
  group_rows("Experimenter demand outcomes", 30, 33, italic = T, bold = F) |>
  #row_spec()
  add_header_above(c(" "= 1,
                     "N"= 1,
                     "N"= 1,
                     "IPV"= 1,
                     "No IPV"= 1,
                     "Difference"= 1,
                     "P-value"= 1,
                     "IPV"= 1,
                     "No IPV"= 1,
                     "Difference"= 1,
                     "P-value" = 1), line = FALSE) |>
  add_header_above(c(" " = 3, "Unadjusted" = 4, "Adjusted" = 4)) |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) |>
  save_kable(
    file = "6_tables/by_baseline_ipv.tex"
  )

# subgroup_tables_w_covs_ipw <- 
#   map(list(c(2,3), c(1)),
#       function(s) {
#         fits <- map(
#           outcomes,
#           function(x) {
#             y_covs <- y_selected_strata$covariate[
#               y_selected_strata$outcome == x &
#                 y_selected_strata$strata == s[1]
#             ]
#             # r_covs <- r_selected_strata$covariate[
#             #   r_selected_strata$outcome == x & 
#             #     r_selected_strata$strata == s[1]
#             # ]
#             z_covs <- z_selected_strata$covariate[
#               z_selected_strata$strata == s[1]
#             ]
#             # z_covs <- NULL
#             covs <- unique(c(y_covs, z_covs))
#             # covs <- NULL
#             
#             d <- subset(rmc, id_status_w == 1 & strata_new %in% s)
#             if (length(covs) > 0) {
#               lm_robust(
#                 formula = reformulate(
#                   termlabels = c(
#                     "treatment",
#                     covs,
#                     paste0("treatment:", covs)
#                   ),
#                   response = x
#                 ),
#                 data = d,
#                 weights = response_weights_strata[[s[1]]][[x]]
#               ) 
#               
#             } else{
#               lm_robust(
#                 formula = reformulate(
#                   termlabels = c(
#                     "treatment"
#                   ),
#                   response = x
#                 ),
#                 data = d,
#                 weights = response_weights_strata[[s[1]]][[x]]
#               )
#             }
#             
#           })
#         
#         names(fits) <- outcomes
#         tab <- bind_rows(lapply(fits, tidy))
#         
#         tab |>
#           filter(term %in% c("treatment")) |>
#           select(term, outcome, estimate, std.error, p.value) |>
#           mutate(strata = if_else(s[1] == 2, "group1", "group2"))
#       })
# 
# bind_rows(subgroup_tables_w_covs_ipw) |>
#   pivot_wider(
#     names_from = strata, 
#     values_from = c(estimate, std.error, p.value)
#   ) |>
#   mutate(
#     outcome = outcome_labels,
#     p.value_diff = pnorm(
#       -abs(estimate_group1 - estimate_group2) / 
#         sqrt(std.error_group1^2 + std.error_group2^2)) * 2,
#     estimate_diff = add_stars(estimate_group1 - estimate_group2, p.value_diff), 
#     estimate_group2 = add_stars(estimate_group2, p.value_group2),
#     estimate_group1 = add_stars(estimate_group1, p.value_group1)
#   ) |>
#   select(outcome,
#          estimate_group1,
#          estimate_group2,
#          estimate_diff,
#          p.value_diff) |>
#   kable(
#     format = "latex",
#     digits = 3,
#     col.names = c(
#       "Outcome",
#       c("{Baseline IPV}", "{No baseline IPV}"),
#       "{Difference (1-2)}",
#       "P-value"
#     ),
#     align = c("l", "d", "d", "d", "c"),
#     booktabs = TRUE,
#     linesep = '',
#     escape = FALSE
#   ) |>
#   kable_styling(latex_options = "hold_position") |>
#   group_rows("Violence outcomes", 1, 12) |>
#   group_rows("Other primary outcomes", 13, 15) |>
#   group_rows("Secondary outcomes", 16, 26) |>
#   group_rows("Experimenter demand outcomes", 27, 30) |>
#   add_header_above(c(" " = 1, "(1)" = 1, "(2)" = 1, " " = 1, " " = 1)) |>
#   save_kable(
#     file = "6_tables/by_baseline_ipv_wcovs_ipw.tex"
#   )


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
         p.value_diff) |>
  filter(!str_detect(outcome, "Z-score"))

kable(
  x = tab,
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
  caption = "Effect of HEP among couples reporting physical or sexual IPV at baseline compared to those reporting no physical or sexual IPV violence at baseline after splitting those who did not report violence into low and high propensity couples \\label{tab:hetfx_propensity}",
  align = c("l", "D", "d", "D", "d", "D", "d", "c"),
  booktabs = TRUE,
  linesep = '',
  escape = FALSE
) |>
  kable_styling(latex_options = "hold_position") |>
  group_rows("Violence outcomes", 1, 12, italic = T, bold = F) |>
  group_rows("Other primary outcomes", 13, 15, italic = T, bold = F) |>
  group_rows("Secondary outcomes", 16, 26, italic = T, bold = F) |>
  group_rows("Experimenter demand outcomes", 27, 30, italic = T, bold = F) |>
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
    file = "6_tables/by_baseline_propensity.tex"
  )

         


         # fits1 <- map(
         #   outcomes,
         #   function(x) {
         #     lm_robust(
         #       formula = reformulate(
         #         termlabels = c("treatment"),
         #         response = x
         #       ),
         #       data = subset(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
         #     )
         #   })
         # 
         # pvalues1 <- sapply(
         #   fits1, 
         #   function(x) pull(filter(tidy(x), term == "treatment"), "p.value")
         # )
         # 
         # names(fits) <- outcomes
         # 
         # tab <- bind_rows(lapply(fits, tidy))
         # 
         # tab |>
         #   filter(term %in% c("treatment", paste0("treatment:", group, "TRUE"))) |>
         #   mutate(term = case_when(term == "treatment" ~ "group1", TRUE ~ "group2")) |>
         #   select(term, outcome, estimate, p.value) |>
         #   pivot_wider(names_from = term, values_from = c(estimate, p.value)) |>
         #   mutate(
         #     outcome = outcome_labels,
         #     p.value_diff = p.value_group2,
         #     estimate_diff = add_stars(estimate_group2, p.value_diff), 
         #     estimate_group2 = add_stars(estimate_group1 + estimate_group2, pvalues1),
         #     estimate_group1 = add_stars(estimate_group1, p.value_group1)
         #   ) |>
         #   select(outcome,
         #          estimate_group2,
         #          estimate_group1,
         #          estimate_diff,
         #          p.value_diff)
         
       # })


itt_refusals <- 
  map2(
    rep(c("any_ipv_refusals"), each = 2),
    rep(c(FALSE, TRUE), 1),
    function(outcome, adjusted) {
      y_covs <- y_selected_strata$covariate[
        y_selected_strata$outcome == outcome &
          y_selected_strata$strata == 1
      ]
      r_covs <- r_selected_strata$covariate[
        r_selected_strata$strata == 1
      ]
      z_covs <- z_selected_strata$covariate[
        z_selected_strata$strata == 1
      ]
      covs <- unique(c(y_covs, r_covs, z_covs))
      
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
    r_covs <- r_selected_strata$covariate[
      r_selected_strata$strata == 1
    ]
    z_covs <- z_selected_strata$covariate[
      z_selected_strata$strata == 1
    ]
    covs <- unique(c(y_covs, r_covs, z_covs))
    
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

# data-driven -------------------------------------------------------------

# get_cate <- function (outcome, covariates, treatment = "treatment", data, ...) {
#   cc <- which(!is.na(data[[outcome]]))
#   
#   W <- data[[treatment]][cc]
#   Y <- data[[outcome]][cc]
#   X <- data[cc, covariates]
# 
#   tau <- causal_forest(X, Y, W, ...)
#   
#   return(tau)
# }
# 
# 
# cfs <- map(
#   violence_outcomes,
#   function(outcome) {
#     strata_FE <- paste0("strata_new_", 2:7, "_c")
#     y_covs <- y_selected$covariate[y_selected$outcome == outcome]
#     y_covs <- c(y_covs, strata_FE)
#     if (!length(y_covs) == 0) {
#       cf <- get_cate(
#         outcome = outcome,
#         covariates = y_covs,
#         data = subset(rmc, id_status_w == 1)
#       )
#     } else {
#       NULL
#     }
#   }
# )
# 
# 
# cc <- which(!is.na(rmc[["any_ipv"]]))
# train <- sample(cc, length(cc) / 2)
# test <- cc[!cc %in% train]
# X <- rmc[, c(
#   bl_covariates,# y_selected$covariate[y_selected$outcome == "any_ipv"], 
#   paste0("strata_new_", 2:7)
# )]
# 
# forest.Y <- regression_forest(
#   X = X[train, ], 
#   Y = rmc[["any_ipv"]][train], 
#   tune.parameters = "all"
#   )
# 
# forest.Y.varimp <- variable_importance(forest.Y)
# selected.vars <- which(forest.Y.varimp / mean(forest.Y.varimp) > 0.75)
# 
# cf <- causal_forest(
#   X = X[cc, selected.vars],
#   Y = rmc[["any_ipv"]][cc],
#   W = rmc[["treatment"]][cc],
#   W.hat = 0.5,
#   tune.parameters = "all"
# )
# 
# test_calibration(cf)
# priority.cate <- -1 * predict(cf, X[test, selected.vars])$predictions
# 
# cf.eval <- causal_forest(
#   X = X[test, selected.vars],
#   Y = rmc[["any_ipv"]][test],
#   W = rmc[["treatment"]][test],
#   tune.parameters = "all"
# )
# 
# rate <- rank_average_treatment_effect(cf.eval, priority.cate)
# 
# plot(rate)
