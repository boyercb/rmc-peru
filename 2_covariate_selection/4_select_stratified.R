# double post-selection ---------------------------------------------------

lassocovs_s1 <-
  bl_covariates_s1[!bl_covariates_s1 %in% c("any_ipv_bl_c_s1",
                                            "any_physical_bl_c_s1",
                                            "any_sexual_bl_c_s1",
                                            "any_psychological_bl_c_s1")]
lassocovs_s2 <-
  bl_covariates_s2[!bl_covariates_s2 %in% c("any_ipv_bl_c_s2",
                                            "any_physical_bl_c_s2",
                                            "any_sexual_bl_c_s2",
                                            "any_psychological_bl_c_s2")]
lassocovs_edu1 <- 
  bl_covariates_edu1[!bl_covariates_edu1 %in% c("any_ipv_bl_c_edu1",
                                                "any_physical_bl_c_edu1",
                                                "any_sexual_bl_c_edu1",
                                                "any_psychological_bl_c_edu1")]
lassocovs_edu0 <- 
  bl_covariates_edu0[!bl_covariates_edu0 %in% c("any_ipv_bl_c_edu0",
                                                "any_physical_bl_c_edu0",
                                                "any_sexual_bl_c_edu0",
                                                "any_psychological_bl_c_edu0")]
lassocovs_alc1 <- 
  bl_covariates_alc1[!bl_covariates_alc1 %in% c("any_ipv_bl_c_alc1",
                                                "any_physical_bl_c_alc1",
                                                "any_sexual_bl_c_alc1",
                                                "any_psychological_bl_c_alc1")]
lassocovs_alc0 <- 
  bl_covariates_alc0[!bl_covariates_alc0 %in% c("any_ipv_bl_c_alc0",
                                                "any_physical_bl_c_alc0",
                                                "any_sexual_bl_c_alc0",
                                                "any_psychological_bl_c_alc0")]

lassocovs_att1 <- 
  bl_covariates_att1[!bl_covariates_att1 %in% c("any_ipv_bl_c_att1",
                                                "any_physical_bl_c_att1",
                                                "any_sexual_bl_c_att1",
                                                "any_psychological_bl_c_att1")]
lassocovs_att0 <- 
  bl_covariates_att0[!bl_covariates_att0 %in% c("any_ipv_bl_c_att0",
                                                "any_physical_bl_c_att0",
                                                "any_sexual_bl_c_att0",
                                                "any_psychological_bl_c_att0")]




# select baseline covariates that predict outcome (y ~ x)
y_selected_strata <- 
  map2(
    list(c(2,3), c(1)),
    list(lassocovs_s2, lassocovs_s1),
    function(s, covlist) {
      covs <- 
        lapply(c(outcomes, "any_ipv_refusals", "ipv11_w", "ipv12_w", "sex_consent_w"),
               function(x) postlasso(
                 covariates = covlist,
                 outcome = x, 
                 data = filter(rmc, id_status_w == 1 & strata_new %in% s),
                 fixed_effects = if (s[1] == 2) {
                   c(
                    paste0("batch_", 2:5, "_c_s", s[1])
                   )
                 } else {
                   c(
                     paste0("batch_", 2:5, "_c_s", s[1])
                   )
                 }
               ))
      covs <- bind_rows(covs)
      covs$strata <- s[1]
      covs
    }
  )

y_selected_strata <- bind_rows(y_selected_strata)

y_selected_strata_bl <- 
  postlasso(
    covariates = c(
      bl_covariates[!str_replace(bl_covariates, "_c$", "") %in% colnames(na_indicators)], 
      paste0("batch_", 3:5, "_c_s1")
    ),
    outcome = "any_ipv_refusals_bl", 
    data = filter(rmc, ipv_short_form_w_bl == 0 & batch != 1 & strata_new == 1)
  )

y_selected_strata_bl$strata <- 1
  
# select baseline covariates that predict treatment (z ~ x)
z_selected_strata <- 
  map2(
    list(c(2,3), c(1)),
    list(lassocovs_s2, lassocovs_s1),
    function(s, covlist) {
      covs <- postlasso(
        covariates = covlist,
        outcome = "treatment", 
        data = filter(rmc, id_status_w == 1 & strata_new %in% s),
        fixed_effects = if (s[1] == 2) {
          c(
            paste0("batch_", 2:5, "_c_s", s[1])
          )
        } else {
          c(
            paste0("batch_", 2:5, "_c_s", s[1])
          )
        }
      )
      covs <- bind_rows(covs)
      covs$strata <- s[1]
      covs
    }
  )

z_selected_strata <- bind_rows(z_selected_strata)


# education ---------------------------------------------------------------

edu_strata <-
  list("I(rmc$education_m_bl < 6)", "I(rmc$education_m_bl >= 6)")

# select baseline covariates that predict outcome (y ~ x)
y_selected_strata_edu <- 
  map2(
    seq_along(edu_strata),
    list(lassocovs_edu0, lassocovs_edu1),
    function(i, covlist) {
      covs <- 
        lapply(c(violence_outcomes[1:3]),
               function(x) postlasso(
                 covariates = covlist,
                 outcome = x, 
                 data = rmc[eval(parse(text = paste0("rmc$id_status_w == 1 & ", edu_strata[[i]]))), ],
                 fixed_effects = c(
                   # "treatment",
                   paste0("strata_new_", 2:3, "_c_edu", i - 1),
                   paste0("batch_", 2:5, "_c_edu", i - 1)
                 )
               ))
      covs <- bind_rows(covs)
      covs$strata <- i - 1
      covs
    }
  )

y_selected_strata_edu <- bind_rows(y_selected_strata_edu)

# select baseline covariates that predict treatment (z ~ x)
z_selected_strata_edu <- 
  map2(
    seq_along(edu_strata),
    list(lassocovs_edu0, lassocovs_edu1),
    function(i, covlist) {
      covs <- postlasso(
        covariates = covlist,
        outcome = "treatment", 
        data = rmc[eval(parse(text = paste0("rmc$id_status_w == 1 & ", edu_strata[[i]]))), ],
        fixed_effects =  c(
          paste0("strata_new_", 2:3, "_c_edu", i - 1),
          paste0("batch_", 2:5, "_c_edu", i - 1)
        )
      )
      covs <- bind_rows(covs)
      covs$strata <- i - 1
      covs
    }
  )

z_selected_strata_edu <- bind_rows(z_selected_strata_edu)


# alcohol -----------------------------------------------------------------

alc_strata <-
  list("I(rmc$alcohol_man_w_bl == 1)", "I(rmc$alcohol_man_w_bl > 1)")

# select baseline covariates that predict outcome (y ~ x)
y_selected_strata_alc <- 
  map2(
    seq_along(alc_strata),
    list(lassocovs_alc0, lassocovs_alc1),
    function(i, covlist) {
      covs <- 
        lapply(c(violence_outcomes[1:3]),
               function(x) postlasso(
                 covariates = covlist,
                 outcome = x, 
                 data = rmc[eval(parse(text = paste0("rmc$id_status_w == 1 & rmc$batch != 6 &  ", alc_strata[[i]]))), ],
                 fixed_effects = c(
                   paste0("strata_new_", 2:3, "_c_alc", i - 1),
                   paste0("batch_", 2:5, "_c_alc", i - 1)
                 )
               ))
      covs <- bind_rows(covs)
      covs$strata <- i - 1
      covs
    }
  )

y_selected_strata_alc <- bind_rows(y_selected_strata_alc)

# select baseline covariates that predict treatment (z ~ x)
z_selected_strata_alc <- 
  map2(
    seq_along(alc_strata),
    list(lassocovs_alc0, lassocovs_alc1),
    function(i, covlist) {
      covs <- postlasso(
        covariates = covlist,
        outcome = "treatment", 
        data = rmc[eval(parse(text = paste0("rmc$id_status_w == 1 & rmc$batch != 6 &  ", alc_strata[[i]]))), ],
        fixed_effects =  c(
          paste0("strata_new_", 2:3, "_c_alc", i - 1),
          paste0("batch_", 2:5, "_c_alc", i - 1)
        )
      )
      covs <- bind_rows(covs)
      covs$strata <- i - 1
      covs
    }
  )

z_selected_strata_alc <- bind_rows(z_selected_strata_alc)


# tolerance of violence ---------------------------------------------------


att_strata <-
  list("rmc$tolerance_vaw_any_bl == 0", "rmc$tolerance_vaw_any_bl == 1")

# select baseline covariates that predict outcome (y ~ x)
y_selected_strata_att <- 
  map2(
    seq_along(att_strata),
    list(lassocovs_att0, lassocovs_att1),
    function(i, covlist) {
      covs <- 
        lapply(c(violence_outcomes[1:3], "tolerance_vaw_any", "tolerance_vaw_index", mechanisms),
               function(x) postlasso(
                 covariates = covlist,
                 outcome = x, 
                 data = rmc[eval(parse(text = paste0("rmc$id_status_w == 1 & ", att_strata[[i]]))), ],
                 fixed_effects = c(
                   paste0("strata_new_", 2:3, "_c_att", i - 1),
                   paste0("batch_", 2:5, "_c_att", i - 1)
                 )
               ))
      covs <- bind_rows(covs)
      covs$strata <- i - 1
      covs
    }
  )

y_selected_strata_att <- bind_rows(y_selected_strata_att)

# select baseline covariates that predict treatment (z ~ x)
z_selected_strata_att <- 
  map2(
    seq_along(att_strata),
    list(lassocovs_att0, lassocovs_att1),
    function(i, covlist) {
      covs <- postlasso(
        covariates = covlist,
        outcome = "treatment", 
        data = rmc[eval(parse(text = paste0("rmc$id_status_w == 1 & ", att_strata[[i]]))), ],
        fixed_effects =  c(
          paste0("strata_new_", 2:3, "_c_att", i - 1),
          paste0("batch_", 2:5, "_c_att", i - 1)
        )
      )
      covs <- bind_rows(covs)
      covs$strata <- i - 1
      covs
    }
  )

z_selected_strata_att <- bind_rows(z_selected_strata_att)


y_selected_mech <- 
  lapply(c(mechanisms),
         function(x) postlasso(
           covariates = lassocovs,
           outcome = x, 
           data = filter(rmc, treatment == 1),
           fixed_effects = c(
             paste0("batch_", 2:6, "_c")
           )
         )
  )

y_selected_mech <- bind_rows(y_selected_mech)


# attrition ---------------------------------------------------------------

# select baseline covariates that predict response (r ~ x)
r_selected_strata <- 
  map2(
    list(c(2,3), c(1)),
    list(lassocovs_s2, lassocovs_s1),
    function(s, covlist) {
      covs <- postlasso(
        covariates = covlist,
        outcome = "responded_w", 
        data = filter(rmc, strata_new %in% s),
        fixed_effects = if (s[1] == 2) {
          c(
            # paste0("strata_new_", 3, "_c_s", s[1]),
            paste0("batch_", 2:5, "_c_s", s[1])
          )
        } else {
          c(
            paste0("batch_", 2:5, "_c_s", s[1])
          )
        }
      )
      covs <- bind_rows(covs)
      covs$strata <- s[1]
      covs
    }
  )

r_selected_strata <- bind_rows(r_selected_strata)

response_weights_strata <- 
  lapply(list(c(1), c(2,3)),
         function(s) {
           f <- lapply(outcomes, 
                  function(x) {
                    y_covs <- y_selected_strata$covariate[
                      y_selected_strata$outcome == x & 
                        y_selected_strata$strata == s[1]
                    ]
                    
                    y_covs <- y_covs
                    
                    r_covs <- r_selected_strata$covariate[
                      r_selected_strata$strata == s[1]
                    ]
                  
                    covs <- unique(c(y_covs, r_covs))
                    if (length(y_covs) > 0) {
                      fit_n <- glm(
                        formula = reformulate(
                          termlabels = c("1"),
                          response = "responded_w"
                        ),
                        family = binomial(link = "logit"),
                        data = subset(rmc, strata_new %in% s)
                      )
                      
                      fit_d <- glm(
                        formula = reformulate(
                          termlabels = c(
                            "treatment",
                            covs,
                            paste0("treatment:", covs)
                          ),
                          response = "responded_w"
                        ),
                        family = binomial(link = "logit"),
                        data = subset(rmc, strata_new %in% s)
                      )
                      
                      p_n <- predict(
                        fit_n, 
                        newdata = subset(rmc, id_status_w == 1 & strata_new %in% s),
                        type = "response"
                      )
                      p_d <- predict(
                        fit_d, 
                        newdata = subset(rmc, id_status_w == 1 & strata_new %in% s),
                        type = "response"
                      )
                      w <- p_n / p_d
                    } else {
                      fit <- NULL
                      w <- rep(1, nrow(subset(rmc, id_status_w == 1 & strata_new %in% s)))
                    }
                    w
                  })
           names(f) <- outcomes
           f
         })
