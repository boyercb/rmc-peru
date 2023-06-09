make_report_table <- function(models, outcomes, outcome_labels, treatment, data) {
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
  
  attr(rows, 'position') <- c(3, 4, 5, 6)
  
  gm <- tribble(
    ~raw,        ~clean,      ~fmt,
    "nobs", "Observations", function(x) paste0("{", x, "}"),
    "r.squared", "R$^2$", function(x) paste0("{", specd(x, 3), "}")
  )
  
  header <- c(1, rep(2, length(outcomes)))
  names(header) <- c(" ", outcome_labels)
  
  modelsummary(
    models = models,
    output = 'latex',
    coef_omit = "^(?!treatment$)",
    gof_omit = "(AIC)|(BIC)|(RMSE)",
    add_rows = rows,
    align = paste0('l', str_flatten(rep('d', length(models)))),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = gm
  ) |>
    kable_styling(latex_options = "hold_position") |>
    add_header_above(header) 
}
