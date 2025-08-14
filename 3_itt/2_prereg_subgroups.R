
# create grid of regression specifications for pre-registered outcomes
prereg_subgroup_grid <- 
  expand_grid(
    statistic = c(
      function(x) x$statistic[2],
      function(x) x$coefficients[2]
    ),
    outcome = violence_outcomes[1:3],
    subgroup = list(
      ""
    ),
    adjusted = c(
      FALSE,
      TRUE
    )
  )

# vector of indices of reference frame for each analysis
prereg_subgroup_grid <- 
  prereg_subgroup_grid |>
  mutate(
    indices = map(outcome, \(x) {
      if (str_detect(x, "_m$")) {
        !is.na(rmc[[x]]) & !is.na(rmc$id_status_m)
      } else {
        !is.na(rmc[[x]])
      }
    })
  )


# run all analyses and calculate RI p-values
prereg_subgroup_results <- 
  pmap(prereg_subgroup_grid,
       \(outcome, adjusted, statistic, indices) {
         
         fit <-
           main_estimator(
             outcome = outcome,
             covariates = lasso_selections[[outcome]],
             data = rmc[indices, ]
           )
         
         ri(
           fit,
           perms[indices, ],
           stat = statistic
         )
       }, .progress = TRUE)

