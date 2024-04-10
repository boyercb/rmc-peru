group_level_treatments <- list(
  NULL,
  "msg_g_std",
  "msg_comm_emo_reg_g_std",
  "msg_other_g_std",
  "msg_comm_emo_reg_g_conf5_std",
  "msg_comm_emo_reg_g_conf5_n_std",
  NULL,
  "rmc_att4_prop_std",
  NULL,
  "rmc_att2_prop_std",
  NULL,
  "rmc_att1_prop_std",
  NULL,
  "rmc_conf5_prop_std",
  NULL,
  "rmc_att_any_prop_std",
  NULL,
  "rmc_att_index_prop_std"
)

group_level_labels <- list(
  "msg_g_std" = "group messages",
  "msg_comm_emo_reg_g_std" = "group messages: comm.",
  "msg_other_g_std" = "group messages: other",
  "msg_comm_emo_reg_g_conf5_std" = "group messages: infidelity",
  "msg_comm_emo_reg_g_conf5_n_std" = "group messages: no infidelity",
  "rmc_att4_prop_std" = "\\% punish infidelity",
  "rmc_att2_prop_std" = "\\% jealousy is love",
  "rmc_att1_prop_std" = "\\% punish disrespect",
  "rmc_conf5_prop_std" = "\\% argue infidelity",
  "rmc_att_any_prop_std" = "\\% any justifications",
  "rmc_att_index_prop_std" = "justification index"
)

group_level_filenames <- list(
  "group_level_messages_{outcome}",
  "group_level_messages_{outcome}_comm",
  "group_level_messages_{outcome}_infidelity",
  "prop_att4_{outcome}",
  "prop_att2_{outcome}",
  "prop_att1_{outcome}",
  "prop_conf5_{outcome}",
  "prop_att_any_{outcome}",
  "prop_att_index_{outcome}"
)
group_level_titles <- list(
  "by level of group message activity",
  "by level and topic of group message activity",
  "by level and source of group message activity",
  "by composition of group beliefs at baseline",
  "by composition of group beliefs at baseline",
  "by composition of group beliefs at baseline",
  "by composition of group behaviors at baseline",
  "by composition of group beliefs at baseline",
  "by composition of group beliefs at baseline"
)

group_level_outcomes <- c("any_physical", "any_sexual")


# full sample -------------------------------------------------------------

group_level_results <-
  map(group_level_outcomes,
      function(out) {
        f <- map2(rep(group_level_treatments, each = 3),
                  rep(c(0, 1, 2), length(group_level_treatments)),
                  function(tx, adj) {
                    if (!is.null(tx)) {
                      tx <- c("treatment", paste0("treatment:", tx))
                    } else {
                      tx <- c("treatment")
                    }
                    lm_robust(
                      formula = reformulate(
                        termlabels = 
                          if (adj == 0) { 
                            c(tx)
                          } else if (adj == 1) {
                            if (length(tx) == 1) {
                              covs <- c(paste0("batch_", 2:5, "_c"), paste0("strata_new_", 2:4, "_c"))
                            } else {
                              covs <- c(paste0("batch_", 2:5, "_c"), paste0("strata_new_", 2:4, "_c"))
                            } 
                            if (length(tx) > 1) {
                              c(tx, covs)
                            } else {
                              c(tx, covs, paste0("treatment:", covs))
                            }
                          } else if (adj == 2) { 
                            y_covs <- y_selected$covariate[y_selected$outcome == out]
                            z_covs <- z_selected$covariate
                            if (length(tx) == 1) {
                              covs <- c(y_covs, z_covs, paste0("batch_", 2:5, "_c"), paste0("strata_new_", 2:4, "_c"))
                            } else {
                              covs <- c(y_covs, z_covs, paste0("batch_", 2:5, "_c"), paste0("strata_new_", 2:4, "_c"))
                            }
                            if (length(tx) > 1) {
                              c(tx, covs)
                            } else {
                              c(tx, covs, paste0("treatment:", covs))
                            }
                          } ,
                        response = out
                      ),
                      data =  subset(rmc, id_status_w == 1)
                    )
                  }
        )
        names(f) <- rep(paste0("(", 1:6, ")"), length(group_level_treatments)/2)
          
        return(f)
      })

names(group_level_results) <- group_level_outcomes

walk2(
  c("any_physical", "any_sexual"),
  c("any physical violence", "any sexual violence"),
  function(x, y) {
    tx <- "treatment"
    keep <- paste0("treatment:", names(group_level_labels))
    cf <- c("HEP", paste0("HEP $\\times$ ", group_level_labels))
    names(cf) <- c('treatment', paste0("treatment:", names(group_level_labels)))
    
    walk(
      seq_along(group_level_filenames),
      function(i) {
        if (group_level_filenames[[i]] %in% c(
          "group_level_messages_{outcome}_comm",
          "group_level_messages_{outcome}_infidelity"
        )) {
          pos <- 10:12
        } else {
          pos <- 7:9
        }
        make_subgroup_table(
          models = group_level_results[[x]][((i - 1) * 6 + 1):(6 * i)],
          outcomes = rep(x, 2),
          outcome_labels = NULL, 
          treatment = tx,
          keep = keep,
          control = "treatment",
          subgroup = TRUE,
          coef_rename = cf,
          title = paste0(
            "ITT estimates of effects of HEP on ",
            y,
            " in the last 6 months ",
            group_level_titles[[i]],
            ".\\label{tab:",
            str_replace(group_level_filenames[[i]], "\\{outcome\\}", x),
            "}"
          ),
          data = filter(rmc, id_status_w == 1),
          general_note = "",
          rows_pos = pos,
          covs = c("No", "No", "Yes"),
          fe = c("No", "Yes", "Yes")
        ) |>
          save_kable(
            file = paste0("6_tables/group/", str_replace(group_level_filenames[[i]], "\\{outcome\\}", x), ".tex")
          )
      }
    )
  }
)
    

# subgroups ---------------------------------------------------------------

strata <- list(
  "rmc$strata_new %in% c(2, 3)",
  "rmc$strata_new %in% c(1)",
  "I(rmc$education_w_bl >= 6)",
  "I(rmc$education_w_bl < 6)",
  "I(rmc$alcohol_man_w_bl > 1)",
  "I(rmc$alcohol_man_w_bl == 1)"
)

strata_suffix <- list(
  "s2",
  "s1",
  "edu1",
  "edu0",
  "alc1",
  "alc0"
)

strata_titles <- list(
  "among couples reporting IPV at baseline",
  "among couples reporting no IPV at baseline",
  "among couples where woman has above median education at baseline",
  "among couples where woman has below median education at baseline",
  "among couples where man drinks alcohol at baseline",
  "among couples where man doesn't drink alcohol at baseline"
)


selected_covs <- list(
  list(y = y_selected_strata, z = z_selected_strata),
  list(y = y_selected_strata, z = z_selected_strata),
  list(y = y_selected_strata_edu, z = z_selected_strata_edu),
  list(y = y_selected_strata_edu, z = z_selected_strata_edu),
  list(y = y_selected_strata_alc, z = z_selected_strata_alc),
  list(y = y_selected_strata_alc, z = z_selected_strata_alc)
)

group_level_results_by_subgroup <-
  map2(rep(group_level_outcomes, each = length(strata)),
       rep(seq_along(strata), 2),
       function(out, i) {
         f <- map2(rep(group_level_treatments, each = 3),
                   rep(c(0, 1, 2), length(group_level_treatments)),
                   function(tx, adj) {
                     if (!is.null(tx)) {
                       tx <- c("treatment", paste0("treatment:", tx))
                     } else {
                       tx <- c("treatment")
                     }
                     lm_robust(
                       formula = reformulate(
                         termlabels = 
                           if (adj == 0) { 
                             c(tx)
                           } else if (adj == 1) {
                             if (strata_suffix[[i]] %in% c("s1", "s2")) {
                               covs <-
                                 c(
                                   paste0("batch_", 2:5, "_c_", strata_suffix[[i]])
                                 )
                             } else {
                               covs <-
                                 c(
                                   paste0("batch_", 2:5, "_c_", strata_suffix[[i]]),
                                   paste0("strata_new_", 2:3, "_c_", strata_suffix[[i]])
                                 )
                             } 
                             if (length(tx) > 1) {
                               c(tx, covs)
                             } else {
                               c(tx, covs, paste0("treatment:", covs))
                             }
                           } else if (adj == 2) { 
                             last_char <- substr(
                               strata_suffix[[i]], 
                               nchar(strata_suffix[[i]]), 
                               nchar(strata_suffix[[i]])
                              )
                             y_covs <-
                               selected_covs[[i]]$y$covariate[selected_covs[[i]]$y$outcome == out &
                                                                selected_covs[[i]]$y$strata == as.numeric(last_char)]
                             z_covs <-
                               selected_covs[[i]]$z$covariate[selected_covs[[i]]$z$strata == as.numeric(last_char)]
                             
                             if (strata_suffix[[i]] %in% c("s1", "s2")) {
                               covs <-
                                 c(
                                   y_covs,
                                   z_covs,
                                   paste0("batch_", 2:5, "_c_", strata_suffix[[i]])
                                 )
                             } else {
                               covs <- c(
                                 y_covs,
                                 z_covs,
                                 paste0("batch_", 2:5, "_c_", strata_suffix[[i]]),
                                 paste0("strata_new_", 2:3, "_c_", strata_suffix[[i]])
                               )
                             }
                             if (length(tx) > 1) {
                               c(tx, covs)
                             } else {
                               c(tx, covs, paste0("treatment:", covs))
                             }
                           } ,
                         response = out
                       ),
                       data = rmc[eval(parse(text = paste0("rmc$id_status_w == 1 & rmc$batch != 6 &", strata[[i]]))), ]
                     )
                   }
         )
         names(f) <- rep(paste0("(", 1:6, ")"), length(group_level_treatments)/2)
         
         return(f)
       })

names(group_level_results_by_subgroup) <-
  paste0(
    rep(group_level_outcomes, each = length(strata)), 
    "_",
    rep(strata_suffix, length(group_level_outcomes))
  )

subgrp <- rmc[rmc$id_status_w == 1 & rmc$batch != 6, ]

pwalk(
  list(
    x = rep(group_level_outcomes, length(strata)),
    y = rep(c("any physical violence", "any sexual violence"), length(strata)),
    j = rep(seq_along(strata_suffix), each = length(group_level_outcomes))
  ),
  function(x, y, j) {
    tx <- "treatment"
    keep <- paste0("treatment:", names(group_level_labels))
    cf <- c("HEP", paste0("HEP $\\times$ ", group_level_labels))
    names(cf) <- c('treatment', paste0("treatment:", names(group_level_labels)))
    
    walk(
      seq_along(group_level_filenames),
      function(i) {
        if (group_level_filenames[[i]] %in% c(
          "group_level_messages_{outcome}_comm",
          "group_level_messages_{outcome}_infidelity"
        )) {
          pos <- 10:12
        } else {
          pos <- 7:9
        }
        
        print(parse(text = str_replace(strata[[j]], "rmc", "subgrp")))
        make_subgroup_table(
          models = group_level_results_by_subgroup[[paste0(x, "_", strata_suffix[[j]])]][((i - 1) * 6 + 1):(6 * i)],
          outcomes = rep(x, 2),
          outcome_labels = NULL, 
          treatment = tx,
          keep = keep,
          control = "treatment",
          subgroup = eval(parse(text = str_replace(strata[[j]], "rmc", "subgrp"))),
          coef_rename = cf,
          title = paste0(
            "ITT estimates of effects of HEP on ",
            y,
            " in the last 6 months ",
            group_level_titles[[i]],
            " ",
            strata_titles[[j]],
            ".\\label{tab:",
            str_replace(group_level_filenames[[i]], "\\{outcome\\}", x),
            "_",
            strata_suffix[[j]],
            "}"
          ),
          data = filter(rmc, id_status_w == 1 & batch != 6),
          general_note = "",
          rows_pos = pos,
          covs = c("No", "No", "Yes"),
          fe = c("No", "Yes", "Yes")
        ) |>
          save_kable(
            file = paste0(
              "6_tables/group/",
              str_replace(group_level_filenames[[i]], "\\{outcome\\}", x),
              "_",
              strata_suffix[[j]],
              ".tex"
            )
          )
      }
    )
  }
)
    

 