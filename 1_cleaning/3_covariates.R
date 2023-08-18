
# covariate cleaning ------------------------------------------------------

rmc$ipv_controlb_short_w_bl <- zap_labels(rmc$ipv_controlb_short_w_bl)

# recode those who answered short form at baseline
rmc <-
  rmc |>
  mutate(
    ipv2_w_bl = if_else(
      !is.na(ipv_controlb_short_w_bl),
      ipv_controlb_short_w_bl,
      ipv2_w_bl
    ),
    ipv5_w_bl = if_else(
      !is.na(ipv_psycho_short_w_bl),
      ipv_psycho_short_w_bl,
      ipv5_w_bl
    ),
    ipv8_w_bl = if_else(
      !is.na(ipv_physic_short_w_bl), 
      ipv_physic_short_w_bl, 
      ipv8_w_bl
    ),
    ipv11_w_bl = if_else(
      !is.na(ipv_sex1_short_w_bl), 
      ipv_sex1_short_w_bl, 
      ipv11_w_bl
    ),
    ipv12_w_bl = if_else(
      !is.na(ipv_sex2_short_w_bl), 
      ipv_sex2_short_w_bl, 
      ipv12_w_bl
    ),
    ipv_short_form_w_bl = case_when(
      !is.na(ipv_controlb_short_w_bl) | !is.na(ipv_psycho_short_w_bl) |
        !is.na(ipv_physic_short_w_bl) | !is.na(ipv_sex1_short_w_bl) | 
        !is.na(ipv_sex2_short_w_bl) ~ 1,
      is.na(ipv2_w_bl) & is.na(ipv5_w_bl) &
        is.na(ipv7_w_bl) & is.na(ipv11_w_bl) & 
        is.na(ipv12_w_bl) ~ NA,
      TRUE ~ 0
    )
  )

rmc <- 
  rmc |>
  mutate(
    any_ipv_bl = case_when(
      (ipv8_w_bl > 1 | ipv11_w_bl > 1 | ipv12_w_bl > 1) & 
        ipv_short_form_w_bl == 1 ~ 1,
      (ipv8_w_bl== 1 & ipv11_w_bl == 1 & ipv12_w_bl == 1) & 
        ipv_short_form_w_bl == 1 ~ 0,
      (ipv7_w_bl > 1 | ipv8_w_bl > 1 | ipv9_w_bl > 1 | ipv10_w_bl > 1 | 
         ipv11_w_bl > 1) & batch == 1 ~ 1,
      (ipv7_w_bl == 1 & ipv8_w_bl == 1 & ipv9_w_bl == 1 & ipv10_w_bl == 1 & 
         ipv11_w_bl == 1) & batch == 1 ~ 0,
      (ipv7_w_bl > 1 | ipv8_w_bl > 1 | ipv9_w_bl > 1 | ipv10_w_bl > 1 | 
         ipv11_w_bl > 1 | ipv12_w_bl > 1) & batch != 1 ~ 1,
      (ipv7_w_bl == 1 & ipv8_w_bl == 1 & ipv9_w_bl == 1 & ipv10_w_bl == 1 & 
         ipv11_w_bl == 1 & ipv12_w_bl == 1) & batch != 1 ~ 0,
      TRUE ~ NA_integer_
    ), 
    any_psychological_bl =  case_when(
      ipv5_w_bl > 1 & ipv_short_form_w_bl == 1 ~ 1,
      ipv5_w_bl == 1 & ipv_short_form_w_bl == 1 ~ 0,
      ipv5_w_bl > 1 | ipv6_w_bl > 1 ~ 1,
      ipv5_w_bl == 1 & ipv6_w_bl == 1 ~ 0,
      TRUE ~ NA_integer_
    ),
    any_physical_bl = case_when(
      ipv8_w_bl > 1 & ipv_short_form_w_bl == 1 ~ 1,
      ipv8_w_bl == 1 & ipv_short_form_w_bl == 1 ~ 0,
      ipv7_w_bl > 1 | ipv8_w_bl > 1 | ipv9_w_bl > 1 | ipv10_w_bl > 1 ~ 1,
      ipv7_w_bl == 1 & ipv8_w_bl == 1 & ipv9_w_bl == 1 & ipv10_w_bl == 1 ~ 0,
      TRUE ~ NA_integer_
    ),
    any_sexual_bl = case_when(
      ipv11_w_bl > 1 & strata == 4 ~ 1,
      ipv11_w_bl == 1 & strata == 4 ~ 0,
      ipv11_w_bl > 1 | ipv12_w_bl > 1 ~ 1,
      ipv11_w_bl == 1 & ipv12_w_bl == 1 ~ 0,
      TRUE ~ NA_integer_
    ),
    any_ipv_bl = replace(any_ipv_bl, participant_id %in% c(235, 650, 753, 870), 0),
    any_physical_bl = replace(any_physical_bl, participant_id %in% c(235, 650, 753, 870), 0),
    any_sexual_bl = replace(any_sexual_bl, participant_id %in% c(235, 650, 753, 870), 0)
  ) 

rmc$ipv_refusals_bl <- 
  rowSums(is.na(rmc[, paste0("ipv", 7:12, "_w_bl")]))

rmc$ipv_refusals_bl <- ifelse(
  rmc$batch == 1 & is.na(rmc$ipv12_w_bl),
  rmc$ipv_refusals_bl - 1,
  rmc$ipv_refusals_bl
)

rmc$ipv_refusals_bl <- ifelse(
  rmc$batch == 6 & rmc$ipv_refusals_bl == 6,
  0,
  rmc$ipv_refusals_bl
)
  
rmc$any_ipv_refusals_bl <- as.numeric(rmc$ipv_refusals_bl > 0)



rmc <- 
  rmc |>
  rowwise() |>
  mutate(
    psychological_score_bl = mean(c(ipv5_w_bl, ipv6_w_bl), na.rm = TRUE),
    ipv_score_bl = mean(c(ipv7_w_bl, ipv8_w_bl, ipv9_w_bl, ipv10_w_bl, ipv11_w_bl, ipv12_w_bl), na.rm = TRUE),
    physical_score_bl = mean(c(ipv7_w_bl, ipv8_w_bl, ipv9_w_bl, ipv10_w_bl), na.rm = TRUE),
    sexual_score_bl = mean(c(ipv11_w_bl, ipv12_w_bl), na.rm = TRUE),

    psychological_score_bl = (psychological_score_bl - 1) / 4,
    ipv_score_bl = (ipv_score_bl - 1) / 4,
    physical_score_bl = (physical_score_bl - 1) / 4,
    sexual_score_bl = (sexual_score_bl - 1) / 4,
  ) |>
  ungroup()

# create new strata indicators
rmc <-
  rmc |>
  mutate(
    strata_new = case_when(
      strata == 1 ~ 1,
      strata == 2 ~ 2,
      strata == 3 & any_ipv_bl == 0 ~ 1,
      strata == 3 & any_ipv_bl == 1 ~ 3,
      strata == 4 ~ 4
      # strata == 4 & any_ipv_bl == 0 ~ 1,
      # strata == 4 & any_sexual_bl == 1 & any_physical_bl == 0 ~ 2,
      # strata == 4 & any_ipv_bl == 1 ~ 3,
      # strata == 4 & is.na(any_ipv_bl) ~ 4
    )
  )

# recode special 88 and 99 codes
rmc <- 
  rmc |>
  mutate(
    across(
      all_of(c(blw_covariates_cont, blm_covariates_cont)),
      ~replace(.x, .x == 88 | .x == 99, NA)
    )
  )

# include time invariant endline questions in baseline covs
rmc <- 
  rmc |>
  mutate(
    children_num_w_bl = replace(children_num_w, children_num_w == 66, NA),
    children_num5years_w_bl = children_num5years_w,
    pregnant_w_bl = replace(pregnant_w, pregnant_months_w < 20, 0)
  )

time_invariant <- c(
  "children_num_w_bl",
  "children_num5years_w_bl",
  "pregnant_w_bl"
)

blw_covariates_cont <-
  c(blw_covariates_cont,
    "children_num_w_bl",
    "children_num5years_w_bl")

blw_covariates_bin <-
  c(blw_covariates_bin, "ipv_short_form_w_bl", "pregnant_w_bl")


# prep additional covariates ----------------------------------------------


missing_responses <- 
  colSums(is.na(rmc[, c(blm_covariates, blw_covariates)]))

miss_indicators <- 
  names(missing_responses[missing_responses >= 20])

miss_mean_impute <- 
  names(missing_responses[missing_responses > 0 & missing_responses< 20])

# create missingness indicator variables
na_indicators <-
  map_dfc(miss_indicators,
          function(cov, data)
            make_indicate_na(data[[cov]], prefix = cov),
          data = rmc)

# create dummy variables for categorical variables
dummy_variables <-
  map_dfc(c(
    blm_covariates_cat,
    blw_covariates_cat,
    "strata_new",
    "batch",
    "group"
  ),
  function(cov, data) {
    d <- make_dummies(data[[cov]], prefix = cov)
    if (cov %in% miss_mean_impute) {
      for (j in 1:ncol(d)) {
        d[, j] <- ifelse(
          is.na(d[, j]), 
          mean(d[, j], na.rm = TRUE), 
          d[, j]
        )
      }
    }
    d
  },
  data = rmc)

# bind cols
rmc <- bind_cols(rmc, na_indicators, dummy_variables)

# create polynomial transformations of continuous variables
polys <- 
  map_dfc(c(blm_covariates_cont, blw_covariates_cont),
          function(cov, data)
            make_transformations(data[[cov]], prefix = cov),
          data = rmc)

# bind cols
rmc <- bind_cols(rmc, polys)

# get names of all covs
bl_covariates <-
  c(colnames(na_indicators),
    colnames(dummy_variables),
    blm_covariates_bin,
    blw_covariates_bin,
    colnames(polys)
    )

# recode NA to same value
rmc <- rmc |>
  mutate(
    across(
      all_of(c(bl_covariates)), 
      function(x) replace_na(x, 0)
    )
  )

nonzero_responses <- colSums(rmc[, bl_covariates] > 0)
nonzero_responses_s1 <- colSums(rmc[rmc$strata_new %in% c(1), bl_covariates] > 0)
nonzero_responses_s2 <- colSums(rmc[rmc$strata_new %in% c(2, 3), bl_covariates] > 0)

covs_with_few_responses <- names(nonzero_responses)[nonzero_responses <= 20]
covs_with_few_responses_s1 <- names(nonzero_responses_s1)[nonzero_responses_s1 <= 20]
covs_with_few_responses_s2 <- names(nonzero_responses_s2)[nonzero_responses_s2 <= 20]

bl_covariates <- bl_covariates[!bl_covariates %in% covs_with_few_responses]
bl_covariates_s1 <- bl_covariates[!bl_covariates %in% covs_with_few_responses_s1]
bl_covariates_s2 <- bl_covariates[!bl_covariates %in% covs_with_few_responses_s2]

# center covariates
rmc[, paste0(bl_covariates, "_c")] <- 
  scale(rmc[, bl_covariates], center = TRUE, scale = FALSE)

rmc[rmc$strata_new %in% c(2, 3), paste0(bl_covariates_s2, "_c_s2")] <- 
  scale(rmc[rmc$strata_new %in% c(2, 3), bl_covariates_s2], center = TRUE, scale = FALSE)

rmc[rmc$strata_new %in% c(1), paste0(bl_covariates_s1, "_c_s1")] <- 
  scale(rmc[rmc$strata_new %in% c(1), bl_covariates_s1], center = TRUE, scale = FALSE)


bl_covariates_raw <- bl_covariates
bl_covariates <- paste0(bl_covariates, "_c")
bl_covariates_s1 <- paste0(bl_covariates_s1, "_c_s1")
bl_covariates_s2 <- paste0(bl_covariates_s2, "_c_s2")

# remove indicators from list
bl_covariates <- 
  bl_covariates[!str_detect(bl_covariates, "(strata_new)|(batch)|(group)")]

bl_covariates_s1 <- 
  bl_covariates_s1[!str_detect(bl_covariates_s1, "(strata_new)|(batch)|(group)")]

bl_covariates_s2 <- 
  bl_covariates_s2[!str_detect(bl_covariates_s2, "(strata_new)|(batch)|(group)")]

violence_items <- paste0("ipv", 1:15, "_w")

control_items <- 
  c(
    "control_cooking_w",
    "control_visiting_w",
    "control_buying_clothes_w",
    "control_wear_w",
    "control_earnings_w",
    "control_health_care_w",
    "control_big_buys_w"
  )

consent_items <- 
  c(
    "sex_consent_w",
    "sex_refuse_w"
  )

comm_items <- c(
  paste0("communication", 1:11, "_w"),
  paste0("reaction", c(1, 2, 3, 5), "_w"),
  paste0("communication", 1:4, "_m")
  # paste0("conflicts", 1:7, "_m")
)

depression_items <- paste0("health_", 1:6, "_m")


satisfaction_items <- c(
  paste0("satisfaction", 1:4, "_m"),
  paste0("satisfaction", 1:4, "_w")
)


attitude_items_w <- paste0("gem", 1:8, "_w")
attitude_items_m <- paste0("tolerance_vaw_", 1:5, "_m")

argument_items <- paste0("conflicts", 1:7, "_m")

bias_items <- c(
  "bias_v1_w",
  "bias_v1_1_m",
  paste0("bias_v3_", 1:13, "_w"),
  paste0("bias_v3_", 1:13, "_m")
)






