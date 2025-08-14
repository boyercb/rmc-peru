library(randomizr)

rmc$block <- as.numeric(fct_cross(factor(rmc$batch), factor(rmc$strata)))

ra <- declare_ra(
  N = nrow(rmc),
  blocks = rmc$block,
  prob = 0.5
  #block_m_each = as.matrix(table(rmc$block, rmc$treatment))
)

z_vec <- obtain_permutation_matrix(ra)

strata_FE <- c(
  paste0("strata_new_", 2:4, "_c"),
  paste0("batch_", 2:5, "_c")
)

orig <- lm_robust(
  reformulate(
    termlabels = 
      c("treatment", strata_FE, paste0("treatment:", strata_FE)),
    response = "any_sexual",
  ),
  data = filter(rmc, id_status_w == 1)
)      

# orig <- lm_robust(
#   formula = any_sexual ~ treatment, 
#   data = subset(rmc, id_status_w == 1)
# )

# X <- model.matrix(orig, subset(rmc, id_status_w == 1))
# y <- model.response(model.frame(orig))
sample <- rmc$id_status_w == 1 & !is.na(rmc$any_sexual)

X <- as.matrix(rmc[sample, strata_FE])
y <- rmc$any_sexual[sample]

z_r <- sapply(1:ncol(z_vec), function(i) {
  Z <- z_vec[sample, i]
  dmat <- cbind(1, Z, X, Z*X)
    
  fit <-
    lm_robust_fit(
      y,
      dmat,
      cluster = NULL,
      weights = NULL,
      se_type = "HC2",
      has_int = TRUE
    )
  fit$statistic[2]
  #fit$coefficients[2]
})

mean(abs(z_r) > abs(orig$statistic[2]))
mean(z_r < orig$statistic[2])

# mean(abs(z_r) > abs(orig$coefficients[2]))
# mean(z_r < orig$coefficients[2])







