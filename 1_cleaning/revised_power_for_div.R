library(DeclareDesign)
library(progressr)
library(tidyverse)
library(haven)
library(ICC)

RERUN_SIM <- TRUE

# declare design function -------------------------------------------------

# design function for the Real Man Challenge evaluation in Peru
rmc_evaluation <- function(# number of implementation groups
                           n_groups = 5,
                           # number of strata per group
                           n_block_per_group = 3,
                           # number of men per strata
                           n_men_per_block = rep(c(264, 154, 82), 5),
                           # effect size
                           tau = 0.10,
                           # outcome standard deviation
                           sd = 1,
                           # block standard deviation
                           sd_block = 0.50,
                           # couple standard deviation
                           sd_couple = 0.30,
                           # response rate
                           pi_R = 0.9,
                           # compliance rate
                           pi_A = 0.6) {
                           
  
  population <- declare_model(
    groups = fabricatr::add_level(
      N = n_groups
    ),
    blocks = fabricatr::add_level(
      N = n_block_per_group,
      u_b = rnorm(N, mean = 0, sd = sd_block)
    ),
    couples = fabricatr::add_level(
      N = n_men_per_block,
      u_c = rnorm(N, mean = 0, sd = sd_couple)
    ),
    individuals = fabricatr::add_level(
      N = 2,
      sex = c("M", "F"),
      R = rbinom(N, 1, pi_R),
      A = rbinom(N, 1, pi_A),
      u_i = rnorm(N, mean = 0, sd = sqrt(1 - sd_block^2 - sd_couple^2))
    )
  ) 
  
  potential_outcomes <- declare_potential_outcomes(
    Y ~ tau * Z * A + u_b + u_c + u_i
  )
  
  assignment <-
    declare_assignment(
      Z = randomizr::block_and_cluster_ra(blocks = blocks,
                                          clusters = couples,
                                          prob = 0.5),
      legacy = FALSE
    )
  
  estimand <- declare_inquiries(
    ate_women = 
      sum((Y_Z_1 - Y_Z_0) * I(sex == "F")) / 
      sum(I(sex == "F")),
    
    late_women = 
      sum((Y_Z_1 - Y_Z_0) * I(sex == "F" & A == 1)) /
      sum(I(sex == "F" & A == 1)),
    
    ate_couple = mean(Y_Z_1 - Y_Z_0),
    
    late_couple = 
      sum((Y_Z_1 - Y_Z_0) * I(A == 1)) /
      sum(I(A == 1))
  )
  
  reveal_Y <- declare_reveal(Y, c(Z))
  reveal_A <- declare_step(fabricatr::fabricate, AZ = A * Z)

  ols_women <- declare_estimator(
    Y ~ Z,
    model = estimatr::lm_robust,
    term = "Z",
    subset = sex == "F" & R == 1,
    fixed_effects = ~blocks,
    inquiry = "ate_women",
    se_type = "HC2",
    label = "ols_women"
  )
  
  iv_women <- declare_estimator(
    Y ~ AZ | Z,
    model = estimatr::iv_robust,
    term = "AZ",
    subset = sex == "F" & R == 1,
    fixed_effects = ~blocks,
    inquiry = "late_women",
    se_type = "HC2",
    label = "iv_women"
  )
  
  ols_couple <- declare_estimator(
    Y ~ Z,
    model = estimatr::lm_robust,
    clusters = couples,
    fixed_effects = ~blocks,
    term = "Z",
    subset = R == 1,
    se_type = "stata",
    inquiry = "ate_couple",
    label = "ols_couple"
  )
  
  iv_couple <- declare_estimator(
    Y ~ AZ | Z,
    model = estimatr::iv_robust,
    fixed_effects = ~blocks,
    term = "AZ",
    subset = R == 1,
    clusters = couples,
    inquiry = "late_couple",
    se_type = "stata",
    label = "iv_couple"
  )
  
  design <- 
    population + 
    potential_outcomes + 
    assignment +
    estimand +
    reveal_Y +
    reveal_A + 
    ols_women + 
    iv_women +
    ols_couple +
    iv_couple
  
  return(design)
}

rmc_design_comparisons <- expand_design(
  designer = rmc_evaluation,
  expand = TRUE,
  tau = c(0.05, 0.10, 0.15, 0.20),
  pi_R = c(0.75, 1),
  pi_A = c(0.75, 1)
) 

# run simulations ---------------------------------------------------------

if (RERUN_SIM) {

  rmc_sims <- diagnose_design(
    rmc_design_comparisons,
    sims = 1000,
    bootstrap_sims = 100
  )
  
  
}

specd <- function(x, k) trimws(format(round(x, k), nsmall=k))

library(knitr)
library(kableExtra)

rmc_sims$diagnosands_df %>% 
  filter(inquiry == "ate_women") %>%
  mutate(
    power_ci = paste0(
      '(',
      specd(`se(power)`, 2), 
      ")"
    ),
    power = specd(power, 2)
  ) %>%
  select(tau, pi_R, pi_A, power, power_ci) %>%
  pivot_longer(c(power, power_ci)) %>%
  pivot_wider(names_from = tau, values_from = c(value)) %>%
  mutate(
    pi_R = if_else(name == "power_ci", "", specd(pi_R, 1)),
    pi_A = if_else(name == "power_ci", "", specd(pi_A, 1))
  ) %>%
  select(
    pi_R,
    pi_A,
    ends_with("0.05"),
    ends_with("0.1"),
    ends_with("0.15"),
    ends_with("0.2")
  ) %>%
  kable(
    x = .,
    format = "latex",
    col.names = c(
      "Response (\\%)",
      "Compliance (\\%)",
      "$\\tau$ = 0.05",
      "$\\tau$ = 0.10",
      "$\\tau$ = 0.15",
      "$\\tau$ = 0.20"
    ),
    align = "cccccc",
    booktabs = TRUE,
    escape = FALSE,
    linesep = ""
  ) %>%
  kable_styling() %>%
  add_header_above(c(" " = 2, "MDE" = 4))

