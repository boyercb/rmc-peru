postlasso <- function(covariates, outcome, data, fixed_effects = NULL, func = identity) {
  cc <- which(!is.na(func(data[[outcome]])))
  
  if (!is.null(fixed_effects)) {
    fit_fe <- lm.fit(x = as.matrix(data[cc, fixed_effects]), y = func(data[[outcome]])[cc])
    y_res <- resid(fit_fe)
    
    fit <- rlasso(x = data[cc, covariates], y = y_res)
    
  } else {
    fit <- rlasso(x = data[cc, covariates], y = func(data[[outcome]])[cc])
    
  }
  
  selected <- coef(fit)[
    abs(coef(fit)) > 0 & names(coef(fit)) != "(Intercept)"
  ]
  
  ret <- tibble(
    outcome = outcome,
    covariate = names(selected),
    value = selected
  )
  
  return(ret)
}