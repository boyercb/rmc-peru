
if (impute) {
  rmc <- zap_labels(rmc)
  
  impute_vars <- c(
    bl_covariates, 
    violence_items, 
    control_items,
    consent_items,
    comm_items,
    depression_items,
    satisfaction_items,
    attitude_items_m,
    attitude_items_w,
    argument_items,
    bias_items
  )
  
  imputed_df <- 
    mice(
      data = rmc[rmc$id_status_w == 1, impute_vars],
      m = 1,
      seed = 14378651,
      method = "cart"
    )
  
  rmc_imputed <- rmc
  rmc_imputed[rmc_imputed$id_status_w == 1, impute_vars] <- 
    complete(imputed_df)

} else {
  
  rmc_imputed <- rmc
}
