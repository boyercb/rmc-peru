message_mechanisms <- coded_outcomes[c(2, 8, 10)]

mods <- c(
  lapply(
    message_mechanisms,
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
    message_mechanisms,
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
    str_replace(message_mechanisms, "_g$", "_prop"),
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
    str_replace(message_mechanisms, "_g$", "_prop"),
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
    "Share problems" = 2,
    "Helpful feedback" = 2,
    "Negative interaction" = 2
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
    "Share problems (\\\\%)" = 2,
    "Helpful feedback (\\\\%)" = 2,
    "Negative interactions (\\\\%)" = 2
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


mods <- c(
  lapply(
    message_mechanisms,
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
    message_mechanisms,
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
    str_replace(message_mechanisms, "_g$", "_prop"),
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
    str_replace(message_mechanisms, "_g$", "_prop"),
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
    mean(rmc[[x$outcome]][rmc$treatment == 1 ], na.rm = T)
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
    "Helpful feedback" = 2,
    "Negative interaction" = 2
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
  file = paste0("HEP-manuscript/tables/homogeneity1_rev.tex")
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
    "Helpful feedback (\\\\%)" = 2,
    "Negative interactions (\\\\%)" = 2
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
  file = paste0("HEP-manuscript/tables/homogeneity2_rev.tex")
)