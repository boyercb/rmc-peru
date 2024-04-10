# double post-selection ---------------------------------------------------

# select baseline covariates that predict response (r ~ x)
r_selected <- postlasso(
  covariates = lassocovs,
  outcome = "responded_w", 
  data = rmc,
  fixed_effects = c(
    paste0("strata_new_", 2:4, "_c"),
    paste0("batch_", 2:5, "_c")
  )
)

# fit model for inverse probability of response weights
response_weights <- 
  lapply(outcomes, 
         function(x) {
           y_covs <- y_selected$covariate[
             y_selected$outcome == x 
           ]
           r_covs <- r_selected$covariate
           covs <- unique(c(y_covs, r_covs))
           
           if (length(c(y_covs, r_covs)) > 0) {
             fit_n <- glm(
               formula = reformulate(
                 termlabels = c("1"),
                 response = "responded_w"
               ),
               family = binomial(link = "logit"),
               data = rmc
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
               data = rmc
             )
             
             p_n <- predict(
               fit_n, 
               newdata = subset(rmc, id_status_w == 1),
               type = "response"
             )
             p_d <- predict(
               fit_d, 
               newdata = subset(rmc, id_status_w == 1),
               type = "response"
             )
             w <- p_n / p_d
           } else {
             fit <- NULL
             w <- rep(1, nrow(subset(rmc, id_status_w == 1)))
           }
           w
         })

names(response_weights) <- outcomes


