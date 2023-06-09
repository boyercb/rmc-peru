# double post-selection ---------------------------------------------------

# select baseline covariates that predict outcome (y ~ x)
y_selected <- 
  lapply(outcomes, 
         function(x) postlasso(
           covariates = bl_covariates,
           outcome = x, 
           data = rmc,
           fixed_effects = paste0("strata_new_", 1:7)
          )
        )

y_selected <- bind_rows(y_selected)

# select baseline covariates that predict randomization (z ~ x)
z_selected <- postlasso(
  covariates = bl_covariates,
  outcome = "treatment", 
  data = rmc,
  fixed_effects = paste0("strata_new_", 1:7)
)