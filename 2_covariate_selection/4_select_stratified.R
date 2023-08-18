# double post-selection ---------------------------------------------------

# select baseline covariates that predict outcome (y ~ x)
y_selected_strata <- 
  map2(
    list(c(2,3), c(1)),
    list(bl_covariates_s2, bl_covariates_s1),
    function(s, covlist) {
      covs <- 
        lapply(c(outcomes, "any_ipv_refusals"),
               function(x) postlasso(
                 covariates = c(
                   covlist, 
                   paste0("batch_", 1:6, "_c"),
                   paste0("group_", 1:27, "_c")
                 ),
                 outcome = x, 
                 data = filter(rmc, id_status_w == 1 & strata_new %in% s)
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
      paste0("batch_", 1:6, "_c"),
      paste0("group_", 1:27, "_c")
    ),
    outcome = "any_ipv_refusals_bl", 
    data = filter(rmc, ipv_short_form_w_bl == 0 & batch != 1 & strata_new == 1)
  )

y_selected_strata_bl$strata <- 1
  
# select baseline covariates that predict treatment (z ~ x)
z_selected_strata <- 
  map2(
    list(c(2,3), c(1)),
    list(bl_covariates_s2, bl_covariates_s1),
    function(s, covlist) {
      covs <- postlasso(
        covariates = covlist,
        outcome = "treatment", 
        data = filter(rmc, id_status_w == 1 & strata_new %in% s)
      )
      covs <- bind_rows(covs)
      covs$strata <- s[1]
      covs
    }
  )

z_selected_strata <- bind_rows(z_selected_strata)


# select baseline covariates that predict response (r ~ x)
r_selected_strata <- 
  map2(
    list(c(2,3), c(1)),
    list(bl_covariates_s2, bl_covariates_s1),
    function(s, covlist) {
      covs <- postlasso(
        covariates = covlist[!str_detect(
          covlist, 
          paste0("(", paste(time_invariant, collapse = ")|("), ")")
        )],
        outcome = "responded_w", 
        data = filter(rmc, strata_new %in% s),
        logit = TRUE
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
                    
                    y_covs <- y_covs[!str_detect(
                      y_covs, 
                      paste0("(", paste(time_invariant, collapse = ")|("), ")")
                    )]
                    
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
