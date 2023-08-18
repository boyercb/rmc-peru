# double post-selection ---------------------------------------------------

lassocovs <- c(
  bl_covariates 
  # paste0("batch_", 1:6, "_c"),
  # paste0("group_", 1:27, "_c")
)
          
# select baseline covariates that predict outcome (y ~ x)
y_selected <- 
  lapply(outcomes, 
         function(x) postlasso(
           covariates = lassocovs,
           outcome = x, 
           data = filter(rmc, id_status_w == 1),
           fixed_effects = c(
             paste0("strata_new_", 2:4, "_c"),
             paste0("batch_", 2:6, "_c")
           ),
          )
        )

y_selected <- bind_rows(y_selected)

# select baseline covariates that predict randomization (z ~ x)
z_selected <- postlasso(
  covariates = bl_covariates,
  outcome = "treatment", 
  data = filter(rmc, id_status_w == 1),
  fixed_effects = paste0("strata_new_", 1:4)
)