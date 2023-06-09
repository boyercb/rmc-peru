# double post-selection ---------------------------------------------------

# select baseline covariates that predict response (r ~ x)
r_selected <- 
  lapply(outcomes, 
         function(x) postlasso(bl_covariates, x, rmc, func = is.na))

r_selected <- bind_rows(r_selected)
