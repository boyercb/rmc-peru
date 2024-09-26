

strata_FE <- c(
  paste0("strata_new_", 2:4, "_c"),
  paste0("batch_", 2:5, "_c")
)

strata_FE_s2 <- c(
  # paste0("strata_new_", 2:4, "_c"),
  paste0("batch_", 2:5, "_c_s2")
)

strata_FE_edu0 <- c(
  paste0("strata_new_", 2:3, "_c_edu0"),
  paste0("batch_", 2:5, "_c_edu0")
)


table1 <- 
  map2(
    rep(c("any_physical", "any_sexual"), each = 2),
    rep(c(FALSE, TRUE), 2),
    function(outcome, adjusted) {
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
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

table1b <- 
  map2(
    rep(c("ipv11_w", "ipv12_w"), each = 2),
    rep(c(FALSE, TRUE), 2),
    function(outcome, adjusted) {
      y_covs <- y_selected$covariate[y_selected$outcome == outcome]
      z_covs <- z_selected$covariate
      covs <- unique(c(y_covs, z_covs, strata_FE))
      lm_robust(
        reformulate(
          termlabels = if (adjusted) {
            c("treatment", covs, paste0("treatment:", covs))
          } else {
            c("treatment", strata_FE, paste0("treatment:", strata_FE))
          },
          response = if (outcome != "sex_consent_w") {
            paste0("I(", outcome, "> 1)")
          } else {
            outcome
          }
        ),
        data = filter(rmc, id_status_w == 1)
      )      
    }
  )

table2 <- 
  map2(
    rep(c("any_physical", "any_sexual"), each = 2),
    rep(c(FALSE, TRUE), 2),
    function(outcome, adjusted) {
      y_covs <- y_selected_strata$covariate[
        y_selected_strata$outcome == outcome & y_selected_strata$strata == 2
      ]
      z_covs <- z_selected_strata$covariate[z_selected_strata$strata == 2]
      covs <- unique(c(y_covs, z_covs, strata_FE_s2))
      
      lm_robust(
        reformulate(
          termlabels = if (adjusted) {
            c("treatment", covs, paste0("treatment:", covs))
          } else {
            c("treatment", strata_FE_s2, paste0("treatment:", strata_FE_s2))
          },
          response = outcome,
        ),
        data = filter(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
      )
    }
  )

table2b <- 
  map2(
    rep(c("ipv11_w", "ipv12_w"), each = 2),
    rep(c(FALSE, TRUE), 2),
    function(outcome, adjusted) {
      y_covs <- y_selected_strata$covariate[
        y_selected_strata$outcome == outcome & y_selected_strata$strata == 2
      ]
      z_covs <- z_selected_strata$covariate[z_selected_strata$strata == 2]
      covs <- unique(c(y_covs, z_covs, strata_FE_s2))
      
      lm_robust(
        reformulate(
          termlabels = if (adjusted) {
            c("treatment", covs, paste0("treatment:", covs))
          } else {
            c("treatment", strata_FE_s2, paste0("treatment:", strata_FE_s2))
          },
          response = if (outcome != "sex_consent_w") {
            paste0("I(", outcome, "> 1)")
          } else {
            outcome
          }
        ),
        data = filter(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
      )      
    }
  )

table3 <- 
  map2(
    rep(c("any_physical", "any_sexual"), each = 2),
    rep(c(FALSE, TRUE), 2),
    function(outcome, adjusted) {
      y_covs <- y_selected_strata_edu$covariate[
        y_selected_strata_edu$outcome == outcome & y_selected_strata_edu$strata == 0
      ]
      z_covs <- z_selected_strata_edu$covariate[z_selected_strata_edu$strata == 0]
      covs <- unique(c(y_covs, z_covs, strata_FE_edu0))
      
      # lm_lin(
      #   formula = reformulate(
      #     termlabels = "treatment",
      #     response = outcome
      #   ),
      #   covariates = reformulate(
      #     termlabels = if (adjusted) {
      #       c(covs)
      #     } else {
      #       c(strata_FE_edu0)
      #     }
      #   ),
      #   data = filter(rmc, id_status_w == 1 & I(education_m_bl <= 6))
      # )
      # 
      lm_robust(
        reformulate(
          termlabels = if (adjusted) {
            c("treatment", covs, paste0("treatment:", covs))
          } else {
            c("treatment", strata_FE_edu0, paste0("treatment:", strata_FE_edu0))
          },
          response = outcome,
        ),
        data = filter(rmc, id_status_w == 1 & I(education_m_bl < 6))
      )
    }
  )

make_publication_table(
  models = table1,
  outcomes = c("any_physical", "any_sexual"),
  labels = violence_labels[2:3],
  treatment = "treatment",
  coef_rename = c('treatment' = 'HEP'),
  title = "Effects on violence outcomes \\label{tab:table1}",
  data = rmc
) |>
  save_kable(
    file = "HEP-manuscript/tables/table1.tex"
  )

rmc$any_ipv11_w <- as.numeric(I(rmc$ipv11_w > 1))
rmc$any_ipv12_w <- as.numeric(I(rmc$ipv12_w > 1))

make_publication_table(
  models = table1b,
  outcomes = c("any_ipv11_w", "any_ipv12_w"),
  labels = c("Forced sex", "Forced other acts"),
  treatment = "treatment",
  coef_rename = c('treatment' = 'HEP'),
  title = "Effects on specific items related to sexual violence \\label{tab:table1b}",
  data = rmc
) |>
  save_kable(
    file = "HEP-manuscript/tables/table1b.tex"
  )

make_publication_table(
  models = table2,
  outcomes = c("any_physical", "any_sexual"),
  labels = violence_labels[2:3],
  treatment = "treatment",
  coef_rename = c('treatment' = 'HEP'),
  title = "Effects on violence outcomes among couples experiencing violence at baseline \\label{tab:table2}",
  data = filter(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
) |>
  save_kable(
    file = "HEP-manuscript/tables/table2.tex"
  )

make_publication_table(
  models = table2b,
  outcomes = c("any_ipv11_w", "any_ipv12_w"),
  labels = c("Forced sex", "Forced other acts"),
  treatment = "treatment",
  coef_rename = c('treatment' = 'HEP'),
  title = "Effects on specific items related to sexual violence among couples experiencing violence at baseline \\label{tab:table2b}",
  data = filter(rmc, id_status_w == 1 & strata_new %in% c(2, 3))
) |>
  save_kable(
    file = "HEP-manuscript/tables/table2b.tex"
  )

make_publication_table(
  models = table3,
  outcomes = c("any_physical", "any_sexual"),
  labels = violence_labels[2:3],
  treatment = "treatment",
  title = "Effects on violence outcomes among couples where man has less than high school education \\label{tab:table3}",
  data = filter(rmc, id_status_w == 1 & batch != 6 & I(education_m_bl < 6))
) |>
  save_kable(
    file = "HEP-manuscript/tables/table3.tex"
  )



# 1 -----------------------------------------------------------------------

# is there a relationship between beliefs and sexual violence?
tolerance_atts <- c(
  "punish disrespect" = "I(tolerance_vaw_1_m_bl %in% c(3, 4))",
  "jealousy is love" = "I(tolerance_vaw_2_m_bl %in% c(3, 4))",
  "asking to be harassed" = "I(tolerance_vaw_3_m_bl %in% c(3, 4))",
  "punish infidelity" = "I(tolerance_vaw_4_m_bl %in% c(3, 4))",
  "always willing for sex" = "I(tolerance_vaw_5_m_bl %in% c(3, 4))"
)

mods1 <- 
  lapply(
    c("any_physical_bl", "any_sexual_bl"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = tolerance_atts,
          response = x
        ),
        data = rmc
      )
    }
  )

mods2 <- 
  lapply(
    c("any_physical_bl", "any_sexual_bl"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = "tolerance_vaw_index_bl",
          response = x
        ),
        data = rmc
      )
    }
  )

mods3 <- 
  lapply(
    c("any_physical", "any_sexual"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = "tolerance_vaw_index_bl",
          response = x
        ),
        data = subset(rmc, id_status_w == 1)
      )
    }
  )

mods4 <- 
  lapply(
    c("any_physical", "any_sexual"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = paste0(x, "_bl"),
          response = x
        ),
        data = subset(rmc, id_status_w == 1 & batch != 6)
      )
    }
  )


tolerance_map <-
  c(
    "(Intercept)",
    names(tolerance_atts),
    "justification index",
    "Any sexual violence at baseline",
    "Any physical violence at baseline"
  )
names(tolerance_map) <-
  c(
    "(Intercept)",
    paste0(tolerance_atts, "TRUE"),
    "tolerance_vaw_index_bl",
    "any_sexual_bl",
    "any_physical_bl"
  )

gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "Observations", function(x) paste0("{", x, "}"),
  "r.squared", "R$^2$", function(x) paste0("{", specd(x, 3), "}")
)

ms <- 
  modelsummary(
    models = c(mods1[2], mods2[2], mods3[2], mods4[2]),
    output = 'latex',
    coef_map = tolerance_map,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    align = paste0('l', str_flatten(rep('d', 4))),
    title = "Presence of violence and conservative beliefs among men about when violence is justified predict sexual violence at baseline and endline \\label{tab:att_associations_sex}",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms|>
  kable_styling(latex_options = "hold_position") |>
  add_header_above(c(
    " " = 1,
    "Any sexual: baseline" = 2,
    "Any sexual: endline" = 2
  ), escape = FALSE)

ms <- ms |>
  footnote(
    general = "Note: The first column is a least squares regression of sexual violence at baseline on justification items. The second column shows the same result applied to the index. The third shows persistance of the association regressing endline violence on baseline justification index. The fourth shows the corresponding association between sexual violence at baseline and sexual violence at endline. All standard errors are robust (HC2).",
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/att_associations_sex.tex")
)

ms <- 
  modelsummary(
    models = c(mods1[1], mods2[1], mods3[1], mods4[1]),
    output = 'latex',
    coef_map = tolerance_map,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    align = paste0('l', str_flatten(rep('d', 4))),
    title = "Presence of violence and conservative beliefs among men about when violence is justified predict physical violence at baseline and endline \\label{tab:att_associations_physical}",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms|>
  kable_styling(latex_options = "hold_position") |>
  add_header_above(c(
    " " = 1,
    "Any physical: baseline" = 2,
    "Any physical: endline" = 2
  ), escape = FALSE)

ms <- ms |>
  footnote(
    general = "Note: The first column is a least squares regression of physical violence at baseline on justification items. The second column shows the same result applied to the index. The third shows persistance of the association regressing endline violence on baseline justification index. The fourth shows the corresponding association between physical violence at baseline and physical violence at endline. All standard errors are robust (HC2).",
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/att_associations_physical.tex")
)

# 2 -----------------------------------------------------------------------

mods <- c(
  lapply(
    c("any_physical", "any_sexual", "tolerance_vaw_index"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = c("treatment",
                         "treatment:rmc_att_index_prop_std", 
                         paste0("batch_", 2:5, "_c"),
                         paste0("strata_new_", 2:4, "_c")),
          response = x
        ),
        data = subset(rmc, id_status_w == 1)
      )
    }
  ),
  lapply(
    c("any_physical", "any_sexual", "tolerance_vaw_index"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = c("treatment",
                         "treatment:rmc_att_index_prop_std",
                         paste0("batch_", 2:5, "_c"),
                         paste0("strata_new_", 2:4, "_c")),
          response = x
        ),
        data = subset(rmc, id_status_w == 1 & I(tolerance_vaw_index_bl < median(tolerance_vaw_index_bl)))
      )
    }
  ),
  lapply(
    c("any_physical", "any_sexual", "tolerance_vaw_index"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = c("treatment",
                         "treatment:rmc_att_index_prop_std",
                         paste0("batch_", 2:5, "_c"),
                         paste0("strata_new_", 2:4, "_c")),
          response = x
        ),
        data = subset(rmc, id_status_w == 1 & I(tolerance_vaw_index_bl >= median(tolerance_vaw_index_bl)))
      )
    }
  ),
  lapply(
    c("any_physical", "any_sexual", "tolerance_vaw_index"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = if (str_detect(x, "physical")) {
            c("treatment",
              "treatment:rmc_physical_prop_std",
              paste0("batch_", 2:5, "_c"),
              paste0("strata_new_", 2:3, "_c"))
          } else {
            c("treatment",
              "treatment:rmc_sexual_prop_std",
              paste0("batch_", 2:5, "_c"),
              paste0("strata_new_", 2:3, "_c"))
          },
          response = x
        ),
        data = subset(rmc, id_status_w == 1 & batch != 6)
      )
    }
  ),
  lapply(
    c("any_physical", "any_sexual", "tolerance_vaw_index"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = if (str_detect(x, "physical")) {
            c("treatment",
              "treatment:rmc_physical_prop_std",
              paste0("batch_", 2:5, "_c"))
          } else {
            c("treatment",
              "treatment:rmc_sexual_prop_std",
              paste0("batch_", 2:5, "_c"))
          },
          response = x
        ),
        data = subset(rmc, id_status_w == 1 & I(tolerance_vaw_index_bl < median(tolerance_vaw_index_bl)) & batch != 6)
      )
    }
  ),
  lapply(
    c("any_physical", "any_sexual", "tolerance_vaw_index"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = if (str_detect(x, "physical")) {
            c("treatment",
              "treatment:rmc_physical_prop_std",
              paste0("batch_", 2:5, "_c"))
          } else {
            c("treatment",
              "treatment:rmc_sexual_prop_std",
              paste0("batch_", 2:5, "_c"))
          },
          response = x
        ),
        data = subset(rmc, id_status_w == 1 & I(tolerance_vaw_index_bl >= median(tolerance_vaw_index_bl)) & batch != 6)
      )
    }
  )
)

fit <- lm_robust(
  formula = reformulate(
    termlabels = c("treatment",
                   "treatment:ns(rmc_att_any_prop, 3)", 
                   "I(tolerance_vaw_index_bl > 0)",
                   "treatment:I(tolerance_vaw_index_bl > 0)",
                   "treatment:ns(rmc_att_any_prop, 3):I(tolerance_vaw_index_bl > 0)",
                   paste0("batch_", 2:5, "_c"),
                   paste0("strata_new_", 2:4, "_c")),
    response = "any_sexual"
  ),
  data = subset(rmc, id_status_w == 1)
)

p <- marginaleffects::plot_comparisons(
  fit,
  variables = list(treatment = 1),
  condition = list("rmc_att_any_prop", "tolerance_vaw_index_bl" = 0:1)
) +
  labs(x = "Proportion of men justifying any violence at baseline (group-level)",
       y = "Effect of HEP on sexual violence") +
  scale_color_discrete(name = "", labels = c("No justification", "Any justification")) +
  scale_fill_discrete(name = "", labels = c("No justification", "Any justification")) +
  scale_y_continuous(n.breaks = 8) +
  theme_classic() +
  theme(legend.position = "top")

ggsave(
  filename = "HEP-manuscript/figures/effect_by_comp_any_np.pdf",
  plot = p,
  device = "pdf",
  width = 6.5,
  height = 5
)

dep_var_means <- 
  sapply(mods, function(x) {
    out <- x$outcome
    d <- model.frame(x)
    mean(d[d$treatment == 0, out])
  })

rows <- tibble(
  sample = c("Full", "No just.", "Any just.", "Full", "No just.", "Any just."),
  FE = rep("Yes", 6),
  dep_var_mean = paste0("{", specd(dep_var_means[c(2, 5, 8, 11, 14, 17)], 3), "}"),
)

rows <- as_tibble(t(rows))
rows <- add_column(rows, c("Sample", "Fixed Effects", "Control Mean"), .before = 1)
attr(rows, 'position') <- c(7, 8, 9)

gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "Observations", function(x) paste0("{", x, "}")
)

ms <- 
  modelsummary(
    models = c(
      mods[2],
      mods[5],
      mods[8],
      mods[11],
      mods[14],
      mods[17]
    ),
    output = 'latex',
    statistic = c('({std.error})'),
    coef_map = c(
      "treatment" = "HEP",
      "I(tolerance_vaw_index_bl > 0)TRUE" = "just. index (ind.)",
      # "tolerance_vaw_index_bl" = "just. index (ind.)",
      "treatment:I(tolerance_vaw_index_bl > 0)TRUE" = "HEP $\\times$ just. index (ind.)",
      # "treatment:tolerance_vaw_index_bl" = "HEP $\\times$ just. index (ind.)",
      "treatment:rmc_att_index_prop_std" = "HEP $\\times$ just. index (group)",
      "treatment:rmc_sexual_prop_std" = "HEP $\\times$ \\% sexual violence (group)",
      "treatment:rmc_physical_prop_std" = "HEP $\\times$ \\% physical violence (group)"
    ),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', 6))),
    title = "Effects on sexual violence by group composition.\\label{tab:group_comp_sexual}",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms |>
  kable_styling(font_size = 10) |>
  add_header_above(c(
    " " = 1,
    "Any Sexual" = 6
  ), escape = FALSE)

ms <- ms |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/table4.tex")
)

rows <- tibble(
  sample = c("Full", "No IPV", "Any IPV", "Full", "No just.", "Any just."),
  FE = rep("Yes", 6),
  dep_var_mean = paste0("{", specd(dep_var_means[c(1, 4, 7, 10, 13, 16)], 3), "}"),
)

rows <- as_tibble(t(rows))
rows <- add_column(rows, c("Sample", "Fixed Effects", "Control Mean"), .before = 1)
attr(rows, 'position') <- c(7, 8, 9)

gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "Observations", function(x) paste0("{", x, "}")
)

ms <- 
  modelsummary(
    models = c(
      mods[1],
      mods[4],
      mods[7],
      mods[10],
      mods[13],
      mods[16]
    ),
    output = 'latex',
    statistic = c('({std.error})'),
    coef_map = c(
      "treatment" = "HEP",
      "I(tolerance_vaw_index_bl > 0)TRUE" = "just. index (ind.)",
      # "tolerance_vaw_index_bl" = "just. index (ind.)",
      "treatment:I(tolerance_vaw_index_bl > 0)TRUE" = "HEP $\\times$ just. index (ind.)",
      # "treatment:tolerance_vaw_index_bl" = "HEP $\\times$ just. index (ind.)",
      "treatment:rmc_att_index_prop_std" = "HEP $\\times$ just. index (group)",
      "treatment:rmc_sexual_prop_std" = "HEP $\\times$ \\% sexual violence (group)",
      "treatment:rmc_physical_prop_std" = "HEP $\\times$ \\% physical violence (group)"
    ),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', 6))),
    title = "Effects on physical violence by group composition. \\label{tab:group_comp_physical}",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms |>
  kable_styling(font_size = 10) |>
  add_header_above(c(
    " " = 1,
    "Any Physical" = 6
  ), escape = FALSE)

ms <- ms |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/table4a.tex")
)


rows <- tibble(
  sample = c("Full", "No just.", "Any just."),
  FE = rep("Yes", 3),
  dep_var_mean = paste0("{", specd(dep_var_means[c(3, 6, 9)], 3), "}"),
)

rows <- as_tibble(t(rows))
rows <- add_column(rows, c("Sample", "Fixed Effects", "Control Mean"), .before = 1)
attr(rows, 'position') <- c(4, 5, 6)

gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "Observations", function(x) paste0("{", x, "}")
)

ms <- 
  modelsummary(
    models = c(
      mods[3],
      mods[6],
      mods[9]
    ),
    output = 'latex',
    statistic = c('({std.error})'),
    coef_map = c(
      "treatment" = "HEP",
      "I(tolerance_vaw_index_bl > 0)TRUE" = "just. index (ind.)",
      # "tolerance_vaw_index_bl" = "just. index (ind.)",
      "treatment:I(tolerance_vaw_index_bl > 0)TRUE" = "HEP $\\times$ just. index (ind.)",
      # "treatment:tolerance_vaw_index_bl" = "HEP $\\times$ just. index (ind.)",
      "treatment:rmc_att_index_prop_std" = "HEP $\\times$ just. index (group)",
      "treatment:rmc_sexual_prop_std" = "HEP $\\times$ \\% sexual violence (group)",
      "treatment:rmc_physical_prop_std" = "HEP $\\times$ \\% physical violence (group)"
    ),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', 3))),
    title = "Effects on justification of violence by group composition.\\label{tab:group_comp_just}",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms |>
  kable_styling(font_size = 10) |>
  add_header_above(c(
    " " = 1,
    "Justification Index" = 3
  ), escape = FALSE)

ms <- ms |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/group_comp_just.tex")
)


# mechanisms --------------------------------------------------------------

mods <- 
  c(
    # lapply(
    #   c("remained_in_chat", "msg_g", "msg_comm_emo_reg_g", "msg_other_g"),
    #   function(x) {
    #     lm_robust(
    #       formula = reformulate(
    #         termlabels = c("rmc_att_index_prop_std", 
    #                        "I(tolerance_vaw_index_bl >= median(tolerance_vaw_index_bl))",
    #                        "rmc_att_index_prop_std:I(tolerance_vaw_index_bl >= median(tolerance_vaw_index_bl))",
    #                        paste0("batch_", 2:5, "_c")),
    #         response = x
    #       ),
    #       data = subset(rmc, treatment == 1)
    #     )
    #   }
    # ),
    lapply(
      c("remained_in_chat", "days_in_chat"),
      function(x) {
        lm_robust(
          formula = reformulate(
            termlabels = c("rmc_att_index_prop_std",
                           paste0("batch_", 2:5, "_c")),
            response = x
          ),
          data = subset(rmc, treatment == 1)
        )
      }
    ),
    lapply(
      c("remained_in_chat", "days_in_chat"),
      function(x) {
        lm_robust(
          formula = reformulate(
            termlabels = c("rmc_att_index_prop_std",
                           paste0("batch_", 2:5, "_c")),
            response = x
          ),
          data = subset(rmc, treatment == 1 & I(tolerance_vaw_index_bl < median(tolerance_vaw_index_bl)))
        )
      }
    ),
    lapply(
      c("remained_in_chat", "days_in_chat"),
      function(x) {
        lm_robust(
          formula = reformulate(
            termlabels = c("rmc_att_index_prop_std",
                           paste0("batch_", 2:5, "_c")),
            response = x
          ),
          data = subset(rmc, treatment == 1 & I(tolerance_vaw_index_bl >= median(tolerance_vaw_index_bl)))
        )
      }
    )
  )

dep_var_means <- 
  sapply(mods, function(x) {
    coef(x)[1]
  })

rows <- tibble(
  sample = c(rep("No just.", 2)),
  FE = rep("Yes", 2),
  dep_var_mean = paste0("{", specd(dep_var_means[3:4], 2), "}"),
)

rows <- as_tibble(t(rows))
rows <- add_column(rows, c("Sample", "Fixed Effects", "Control Mean"), .before = 1)
attr(rows, 'position') <- c(3, 4, 5)

gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "Observations", function(x) paste0("{", x, "}")
)

ms <- 
  modelsummary(
    models = c(
      mods[3],
      mods[4]
    ),
    output = 'latex',
    statistic = c('({std.error})'),
    coef_map = c(
      "rmc_att_index_prop_std" = "just. index (group)"
    ),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', 2))),
    title = "Potential mechanisms for effect of group composition on those who do not believe violence is justified.",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms |>
  kable_styling() |>
  add_header_above(c(
    " " = 1,
    "Remain in chat" = 1,
    "Days in chat" = 1
  ), escape = FALSE)

ms <- ms |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/mechanisms_nojust.tex")
)

rows <- tibble(
  sample = c(rep("Any just.", 2)),
  FE = rep("Yes", 2),
  dep_var_mean = paste0("{", specd(dep_var_means[5:6], 2), "}"),
)

rows <- as_tibble(t(rows))
rows <- add_column(rows, c("Sample", "Fixed Effects", "Control Mean"), .before = 1)
attr(rows, 'position') <- c(3, 4, 5)

gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "Observations", function(x) paste0("{", x, "}")
)

ms <- 
  modelsummary(
    models = c(
      mods[5],
      mods[6]
    ),
    output = 'latex',
    statistic = c('({std.error})'),
    coef_map = c(
      "rmc_att_index_prop_std" = "just. index (group)"
    ),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', 2))),
    title = "Potential mechanisms for effect of group composition on those who believe violence is sometimes justified.",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms |>
  kable_styling() |>
  add_header_above(c(
    " " = 1,
    "Remain in chat" = 1,
    "Days in chat" = 1
  ), escape = FALSE)

ms <- ms |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/mechanisms_anyjust_v.tex")
)


rows <- tibble(
  FE = rep("Yes", 2),
  dep_var_mean = paste0("{", specd(dep_var_means[1:2], 2), "}"),
)

rows <- as_tibble(t(rows))
rows <- add_column(rows, c("Fixed Effects", "Control Mean"), .before = 1)
attr(rows, 'position') <- c(3, 4)

gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "Observations", function(x) paste0("{", x, "}")
)

ms <- 
  modelsummary(
    models = c(
      mods[1],
      mods[2]
    ),
    output = 'latex',
    statistic = c('({std.error})'),
    coef_map = c(
      "rmc_att_index_prop_std" = "just. index (group)",
      "I(tolerance_vaw_index_bl >= median(tolerance_vaw_index_bl))TRUE" = "just. index (ind.) $\\geq$ median",
      "rmc_att_index_prop_std:I(tolerance_vaw_index_bl >= median(tolerance_vaw_index_bl))TRUE" = "just. index (group) $\\times$ just. index (ind.) $\\geq$ median"
    ),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', 2))),
    title = "Potential mechanisms for effect of group composition on those who believe violence is sometimes justified.",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms |>
  kable_styling() |>
  add_header_above(c(
    " " = 1,
    "Remain in chat" = 1,
    "Days in chat" = 1
  ), escape = FALSE)

ms <- ms |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/mechanisms.tex")
)



mods <- c(
  lapply(
    c("problem_partner_g", "challenge_beliefs_g", "participants_argue_g"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = c("rmc_att_index_prop_std"),
          response = x
        ),
        data = subset(rmc, treatment == 1),
        clusters = group
      )
    }
  ),
  lapply(
    c("problem_partner_g", "challenge_beliefs_g", "participants_argue_g"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = c("rmc_att_index_prop_std",
                         paste0("batch_", 2:6, "_c")),
          response = x
        ),
        data = subset(rmc, treatment == 1),
        clusters = group
      )
    }
  ),
  lapply(
    c("problem_partner_prop", "challenge_beliefs_prop", "participants_argue_prop"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = c("rmc_att_index_prop_std"),
          response = x
        ),
        data = subset(rmc, treatment == 1),
        clusters = group
      )
    }
  ),
  lapply(
    c("problem_partner_prop", "challenge_beliefs_prop", "participants_argue_prop"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = c("rmc_att_index_prop_std",
                         paste0("batch_", 2:6, "_c")),
          response = x
        ),
        data = subset(rmc, treatment == 1),
        clusters = group
      )
    }
  )
)

dep_var_means <-
  sapply(mods, function(x) {
    mean(rmc[[x$outcome]][rmc$treatment == 1])
  })

rows <- tibble(
  FE = rep(c("No", "Yes"), 3),
  dep_var_mean = paste0("{", specd(dep_var_means[c(1,4,2,5,3,6)], 2), "}")
)

rows <- as_tibble(t(rows))
rows <- add_column(rows, c("Fixed Effects", "Control Mean"), .before = 1)
attr(rows, 'position') <- c(3, 4)

gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "Observations", function(x) paste0("{", x, "}")
)

ms <- 
  modelsummary(
    models = c(
      mods[1],
      mods[4],
      mods[2],
      mods[5],
      mods[3],
      mods[6]
    ),
    output = 'latex',
    statistic = c('({std.error})'),
    coef_map = c(
      # "(Intercept)" = "(Intercept)",
      "rmc_att_index_prop_std" = "just. index (group)"
    ),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', 6))),
    title = "Potential mechanisms for effect of group composition.\\label{tab:mechanisms}",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms |>
  kable_styling() |>
  add_header_above(c(
    " " = 1,
    "Share challenges (\\\\%)" = 2,
    "Helpful feedback (\\\\%)" = 2,
    "Group conflict (\\\\%)" = 2
  ), escape = FALSE)

ms <- ms |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/mechanism1.tex")
)


rows <- tibble(
  FE = rep(c("No", "Yes"), 3),
  dep_var_mean = paste0("{", specd(dep_var_means[c(7,10,8,11,9,12)], 2), "}")
)

rows <- as_tibble(t(rows))
rows <- add_column(rows, c("Fixed Effects", "Control Mean"), .before = 1)
attr(rows, 'position') <- c(3, 4)

gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "Observations", function(x) paste0("{", x, "}")
)

ms <- 
  modelsummary(
    models = c(
      mods[7],
      mods[10],
      mods[8],
      mods[11],
      mods[9],
      mods[12]
    ),
    output = 'latex',
    statistic = c('({std.error})'),
    coef_map = c(
      # "(Intercept)" = "(Intercept)",
      "rmc_att_index_prop_std" = "just. index (group)"
    ),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', 6))),
    title = "Potential mechanisms for effect of group composition.\\label{tab:mechanisms}",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms |>
  kable_styling() |>
  add_header_above(c(
    " " = 1,
    "Share challenges (\\\\%)" = 2,
    "Helpful feedback (\\\\%)" = 2,
    "Group conflict (\\\\%)" = 2
  ), escape = FALSE)

ms <- ms |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/mechanism2.tex")
)




# -------------------------------------------------------------------------


mods <- c(
  lapply(
    c("problem_partner_g", "challenge_beliefs_g", "participants_argue_g"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = c("rmc_homogeneity_prop"),
          response = x
        ),
        data = subset(rmc, treatment == 1),
        clusters = group
      )
    }
  ),
  lapply(
    c("problem_partner_g", "challenge_beliefs_g", "participants_argue_g"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = c("rmc_homogeneity_prop",
                         paste0("batch_", 2:6, "_c")),
          response = x
        ),
        data = subset(rmc, treatment == 1),
        clusters = group
      )
    }
  ),
  lapply(
    c("problem_partner_prop", "challenge_beliefs_prop", "participants_argue_prop"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = c("rmc_homogeneity_prop"),
          response = x
        ),
        data = subset(rmc, treatment == 1),
        clusters = group
      )
    }
  ),
  lapply(
    c("problem_partner_prop", "challenge_beliefs_prop", "participants_argue_prop"),
    function(x) {
      lm_robust(
        formula = reformulate(
          termlabels = c("rmc_homogeneity_prop",
                         paste0("batch_", 2:6, "_c")),
          response = x
        ),
        data = subset(rmc, treatment == 1),
        clusters = group
      )
    }
  )
)

dep_var_means <-
  sapply(mods, function(x) {
    mean(rmc[[x$outcome]][rmc$treatment == 1])
  })

rows <- tibble(
  FE = rep(c("No", "Yes"), 3),
  dep_var_mean = paste0("{", specd(dep_var_means[c(1,4,2,5,3,6)], 2), "}")
)

rows <- as_tibble(t(rows))
rows <- add_column(rows, c("Fixed Effects", "Control Mean"), .before = 1)
attr(rows, 'position') <- c(3, 4)

gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "Observations", function(x) paste0("{", x, "}")
)

ms <- 
  modelsummary(
    models = c(
      mods[1],
      mods[4],
      mods[2],
      mods[5],
      mods[3],
      mods[6]
    ),
    output = 'latex',
    statistic = c('({std.error})'),
    coef_map = c(
      # "(Intercept)" = "(Intercept)",
      "rmc_homogeneity_prop" = "homogeneity (group)"
    ),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', 6))),
    title = "Effect of group homogeneity.\\label{tab:homogeneity}",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms |>
  kable_styling() |>
  add_header_above(c(
    " " = 1,
    "Share problems" = 2,
    "Challenge beliefs" = 2,
    "Participants argue" = 2
  ), escape = FALSE)

ms <- ms |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/homogeneity1.tex")
)


rows <- tibble(
  FE = rep(c("No", "Yes"), 3),
  dep_var_mean = paste0("{", specd(dep_var_means[c(7,10,8,11,9,12)], 2), "}")
)

rows <- as_tibble(t(rows))
rows <- add_column(rows, c("Fixed Effects", "Control Mean"), .before = 1)
attr(rows, 'position') <- c(3, 4)

gm <- tribble(
  ~raw,        ~clean,      ~fmt,
  "nobs", "Observations", function(x) paste0("{", x, "}")
)

ms <- 
  modelsummary(
    models = c(
      mods[7],
      mods[10],
      mods[8],
      mods[11],
      mods[9],
      mods[12]
    ),
    output = 'latex',
    statistic = c('({std.error})'),
    coef_map = c(
      # "(Intercept)" = "(Intercept)",
      "rmc_homogeneity_prop" = "homogeneity (group)"
    ),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', 6))),
    title = "Effect of group homogeneity.\\label{tab:homogeneity}",
    gof_map = gm,
    escape = FALSE
  )

ms <- ms |>
  kable_styling() |>
  add_header_above(c(
    " " = 1,
    "Share problems (\\\\%)" = 2,
    "Challenge beliefs (\\\\%)" = 2,
    "Participants argue (\\\\%)" = 2
  ), escape = FALSE)

ms <- ms |>
  footnote(
    symbol = "* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  )

ms <- gsub(" \\{\\}", " ", ms)
ms <- gsub("\\multicolumn{3}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{5}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{7}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms <- gsub("\\multicolumn{9}{l}{\\rule{0pt}{1em}* p $<$ 0.1, ** p $<$ 0.05, *** p $<$ 0.01}\\\\", "", ms, fixed = TRUE)
ms |> save_kable(
  file = paste0("HEP-manuscript/tables/homogeneity2.tex")
)


ggplot(subset(rmc, treatment == 1),
       aes(x = rmc_att_index_prop_std, y = problem_partner, color = batch)) + 
  geom_point()


geom_smooth()
p1 <- ggplot(subset(rmc, treatment == 1 & tolerance_vaw_index_bl > 0),
         # group_by(group) |> 
         # summarise(
         #   rmc_att_any_prop = mean(rmc_att_any_prop),
         #   participants_argue_prop = mean(participants_argue_prop)
         #   ),
       aes(x = rmc_att_any_prop,
           y = remained_in_chat)) +
  geom_smooth(method = "loess", fill = "lightblue") +
  geom_hline(aes(yintercept = mean(remained_in_chat, na.rm = TRUE)), linetype = "dashed") +
  theme_classic() +
  labs(x = "Proportion of group justifying any violence", 
       y = "Probability of remaining in chat\n(Men justifying any violence)")

p2 <- ggplot(subset(rmc, treatment == 1),
       # group_by(group) |> 
       # summarise(
       #   rmc_att_any_prop = mean(rmc_att_any_prop),
       #   participants_argue_prop = mean(participants_argue_prop)
       #   ),
       aes(x = rmc_att_any_prop, y = participants_argue_i)) + 
  geom_smooth(method = "loess", fill = "lightblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(x = "Proportion of group justifying any violence", 
       y = "Probability of arguing or threaten to leave group")

ggsave(
  filename = "HEP-manuscript/figures/mechanism_plot.pdf",
  plot = p1 + p2,
  device = "pdf",
  width = 10,
  height = 5
)






ggplot(subset(rmc, treatment == 1) |>
       group_by(group) |>
       summarise(
         rmc_att_any_prop = mean(rmc_att_any_prop),
         problem_partner_prop = mean(problem_partner_prop)
         ),
       aes(x = rmc_att_any_prop, y = problem_partner_prop * 100)) + 
  geom_smooth(method = "loess", fill = "lightblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(x = "Proportion of group justifying any violence", 
       y = "Participants argue or threaten to leave group \n(rate per 100 messages)")


ggplot(subset(rmc, treatment == 1),
       aes(x = rmc_att_any_prop, y = challenge_beliefs_prop * 100)) + 
  geom_smooth(method = "gam", fill = "lightblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(x = "Proportion of group justifying any violence", 
       y = "Participants argue or threaten to leave group \n(rate per 100 messages)")
