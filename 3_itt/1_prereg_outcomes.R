
# create grid of regression specifications for pre-registered outcomes
prereg_grid <- 
  expand_grid(
    statistic = c(
      function(x) x$statistic[2],
      function(x) x$coefficients[2]
    ),
    outcome = outcomes,
    adjusted = c(
      FALSE,
      TRUE
    )
  )

# vector of indices of reference frame for each analysis
prereg_grid <- 
  prereg_grid |>
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
prereg_results <- 
  pmap(prereg_grid,
       \(outcome, adjusted, statistic, indices) {

         fit <-
           main_estimator(
             outcome = outcome,
             covariates = if (adjusted) {
               lasso_selections[[outcome]]
             } else {
               NULL
             },
             data = rmc[indices, ]
           )

         ri(
           fit,
           perms[indices, ],
           stat = statistic
         )
       }, .progress = list(
         format = "{prereg_grid$outcome[cli::pb_current]} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
        ))

prereg_final <- prereg_grid
prereg_final$fit <- map(prereg_results, \(x) x$fit)
prereg_final$p.value <- map_dbl(prereg_results, \(x) x$p.value)
