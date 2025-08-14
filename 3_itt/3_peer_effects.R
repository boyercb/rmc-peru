# average direct effect of RMC marginalized over peer assignments
peer_grid <- 
  expand_grid(
    statistic = c(
      function(x) x$statistic[2],
      function(x) x$coefficients[2]
    ),
    outcome = violence_outcomes[1:3],
    adjusted = c(
      FALSE,
      TRUE
    )
  )

peer_grid <-
  mutate(
    peer_grid,
    indices = map(outcome, \(x) !is.na(rmc[[x]]))
  )

# run all analyses and calculate RI p-values
peer_results <- 
  pmap(peer_grid,
       \(outcome, adjusted, statistic, indices) {
         
         fit <-
           main_estimator(
             outcome = outcome,
             covariates = if (adjusted) {
               lasso_selections[[outcome]]
             } else {
               NULL
             },
             data = rmc[indices, ],
             clusters = rmc$clus[indices],
             se_type = "CR2"
           )
         
         ri(
           fit,
           lapply(perms_2stage, \(dat) dat[indices, ]),
           stat = statistic
         )
       }, .progress = list(
         format = "{peer_grid$outcome[cli::pb_current]} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
       ))

peer_final <- peer_grid
peer_final$fit <- map(peer_results, \(x) x$fit)
peer_final$p.value <- map_dbl(peer_results, \(x) x$p.value)


# compositional effects ---------------------------------------------------

comp_grid <- 
  expand_grid(
    statistic = c(
      function(x) x$statistic[c(2,3)],
      function(x) x$coefficients[c(2,3)]
    ),
    outcome = c(
      "any_physical",
      "any_sexual",
      "tolerance_vaw_index"
    ),
    exposure = c(
      "any_sexual_bl",
      "tolerance_vaw_index_bl"
    ),
    subgroup = c(
      "1",
      "tolerance_vaw_any_bl == 0",
      "tolerance_vaw_any_bl == 1"
    ),
    adjusted = c(
      FALSE,
      TRUE
    )
  )


comp_results <- 
  pmap(comp_grid, 
       \(outcome, exposure, statistic, subgroup, adjusted) {
         
         if (adjusted) {
           if (subgroup == "1") {
             adj_set <- lasso_selections[[outcome]]
           } else if (subgroup == "tolerance_vaw_any_bl == 1") {
             adj_set <- lasso_selections_tolerance_vaw_any_bl[[outcome]]
           } else {
             adj_set <- lasso_selections_tolerance_vaw_none_bl[[outcome]]
           }
         } else {
           adj_set <- NULL
         }
         
         
         if (exposure == "any_sexual_bl") {
           fes <- c(
             paste0("strata_new_", 2:3, "_c"),
             paste0("batch_", 2:5, "_c")
           )
         } else {
           fes <- c(
             paste0("strata_new_", 2:4, "_c"),
             paste0("batch_", 2:5, "_c")
           )
         }
         
         if (str_detect(subgroup, " == 0")) {
           fes <- paste0(fes, "_att0")
         } else if (str_detect(subgroup, " == 1")) {
           fes <- paste0(fes, "_att1")
         }
         
         peer_ri(
           outcome = outcome,
           covariates = adj_set,
           peer_exposure = exposure,
           group = "group",
           data = rmc,
           perms = perms_2stage,
           se_type = "stata",
           subgroup = subgroup,
           fe = fes,
           stat = statistic
         )
       }, 
       .progress = list(
         format = "{comp_grid$outcome[cli::pb_current]} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
       ))


comp_final <- comp_grid
comp_final$fit <- map(comp_results, \(x) x$fit)
comp_final$p.value1 <- map_dbl(comp_results, \(x) x$p.value[1])
comp_final$p.value2 <- map_dbl(comp_results, \(x) x$p.value[2])

