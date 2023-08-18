make_report_table <-
  function(models,
           outcomes,
           outcome_labels,
           treatment = "treatment",
           title,
           general_note = "Notes: First column for each outcome is design-based least squares estimator that includes fixed effects for randomization strata and batch. Second column adjusts for baseline covariates selected using double-post-selection lasso. Covariates are mean-centered and interacted with treatment. Heteroscedasticity-consistent robust standard errors (HC2) for all specifications are shown in parentheses and p-values in square brackets.",
           data) {
    dep_var_means <- colMeans(
    data[data[[treatment]] == 0, outcomes],
    na.rm = TRUE
  )
  
  dep_var_sd <- colSds(
    data[data[[treatment]] == 0, outcomes],
    na.rm = TRUE
  )
  
  rows <- tibble(
    covariates = rep(c("No", "Yes"), length(outcomes)),
    FE = rep(c("Yes", "Yes"), length(outcomes)),
    dep_var_mean = rep(paste0("{", specd(dep_var_means, 3), "}"), each = 2),
    dep_var_sd = rep(paste0("{", specd(dep_var_sd, 3), "}"), each = 2),
  )
  
  rows <- as_tibble(t(rows))
  rows <- add_column(rows, c("Covariates", "Fixed Effects", "Control Mean",  "Control SD"), .before = 1)
  
  attr(rows, 'position') <- 4:7
  
  gm <- tribble(
    ~raw,        ~clean,      ~fmt,
    "nobs", "Observations", function(x) paste0("{", x, "}"),
    "r.squared", "R$^2$", function(x) paste0("{", specd(x, 3), "}")
  )
  
  header <- c(1, rep(2, length(outcomes)))
  names(header) <- c(" ", outcome_labels)
  
  ms <- modelsummary(
    models = models,
    output = 'latex',
    statistic = c('({std.error})','[{p.value}]'),
    coef_omit = paste0("^(?!", treatment, "$)"),
    coef_rename = c("remained_in_chat" = 'remained in chat'),
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', length(models)))),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    title = title,
    gof_map = gm, 
    escape = FALSE
  ) 
  ms <- ms|>
    kable_styling(latex_options = "hold_position") |>
    add_header_above(header) |>
    footnote(
      general = general_note,
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
  ms
}
