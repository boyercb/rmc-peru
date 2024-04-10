# double post-selection ---------------------------------------------------

lassocovs <- c(bl_covariates[!bl_covariates %in% c("any_ipv_bl_c",
                                                   "any_physical_bl_c",
                                                   "any_sexual_bl_c",
                                                   "any_psychological_bl_c")])
          
# select baseline covariates that predict outcome (y ~ x)
y_selected <- 
  lapply(c(outcomes, violence_items, control_items, consent_items, comm_items),
         function(x) postlasso(
           covariates = lassocovs,
           outcome = x, 
           data = filter(rmc, id_status_w == 1),
           fixed_effects = c(
             # "treatment",
             paste0("strata_new_", 2:4, "_c"),
             paste0("batch_", 2:5, "_c")
           )
          )
        )

y_selected <- bind_rows(y_selected)

# select baseline covariates that predict randomization (z ~ x)
z_selected <- postlasso(
  covariates = bl_covariates,
  outcome = "treatment", 
  data = filter(rmc, id_status_w == 1),
  fixed_effects = c(
    paste0("strata_new_", 2:4, "_c"),
    paste0("batch_", 2:5, "_c")
  )
)
