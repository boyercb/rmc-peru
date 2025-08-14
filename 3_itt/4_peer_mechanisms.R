mech_grid <- 
  expand_grid(
    statistic = c(
      function(x) x$statistic[c(2)],
      function(x) x$coefficients[c(2)]
    ),
    outcome = c(
      "remained_in_chat",
      "days_in_chat",
      # "msg_i",
      # "msg_char_above_one_i",
      # "msg_word_above_one_i",
      # "msg_word_above_ten_i",
      # "msg_char_above_one_exp",
      # "msg_word_above_one_exp",
      # "msg_word_above_ten_exp",
      # "share_problem_rev_i",
      # "helpful_feedback_i",
      # "share_problem_rev_g",
      # "helpful_feedback_g",
      # "jpr_incite_conflict_g",
      # "share_problem_rev_exp",
      # "helpful_feedback_exp",
      # "jpr_incite_conflict_exp",
      "share_problem_rev_prop",
      "helpful_feedback_prop",
      "jpr_incite_conflict_prop"
      
      # "share_problem_rev_exp_prop",
      # "helpful_feedback_exp_prop",
      # "jpr_incite_conflict_exp_prop"
    ),
    exposure = c(
      "tolerance_vaw_index_bl",
      "tolerance_vaw_any_bl"
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

homo_func <- function(data, group, peer_exposure) {
  data |>
    group_by(.data[[group]]) |>
    mutate(
      ret = (
        sum(.data[[peer_exposure]], na.rm = TRUE) - .data[[peer_exposure]]
      ) / (n() - 1)
    ) |>
    ungroup() |>
    mutate(
      ret = (ret - 0.5)^2,
      # ret = ifelse(
      #   treatment==0, 
      #   0, 
      #   (ret - min(ret)) / (max(ret) - min(ret))
      # )
    ) |>
    pull(ret)
}

peer_func <- function(data, group, peer_exposure) {
  data |>
    group_by(.data[[group]]) |>
    mutate(
      ret = (
        sum(.data[[peer_exposure]], na.rm = TRUE) - .data[[peer_exposure]]
      ) / (n() - 1)
    ) |>
    ungroup() |>
    mutate(
      ret = ifelse(
        treatment==0, 
        0, 
        (ret - min(ret)) / (max(ret) - min(ret))
      )
    ) |>
    pull(ret)
}

mech_results <- 
  pmap(mech_grid, 
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
         
         fes <- paste0("batch_", 2:6, "_c")
         
         if (str_detect(subgroup, " == 0")) {
           fes <- paste0(fes, "_att0")
         } else if (str_detect(subgroup, " == 1")) {
           fes <- paste0(fes, "_att1")
         }
         
         peer_ri(
           outcome = outcome,
           covariates = NULL,
           peer_exposure = exposure,
           group = "group",
           data = rmc,
           perms = perms_group,
           stage = 1,
           se_type = "stata",
           subgroup = subgroup,
           fe = fes,
           stat = statistic,
           peer_func = if (exposure == "tolerance_vaw_any_bl") {
             homo_func
           } else {
             peer_func
           }
         )
       }, 
       .progress = list(
         format = "{mech_grid$outcome[cli::pb_current]} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
       ))


mech_final <- mech_grid
mech_final$fit <- map(mech_results, \(x) x$fit)
mech_final$p.value <- map(mech_results, \(x) x$p.value)