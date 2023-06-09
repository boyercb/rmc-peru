# clean baseline ----------------------------------------------------------

# sort men's variables

# id variable
id_var <- c(
  "participant_id"
)

# covariates - these are things we potentially want to adjust for
blm_covariates <- c(
  "age",
  "years_relationship",
  "cohabiting",
  "hpi",
  "tongue",
  "education",
  "conflicts1",
  "conflicts2",
  "conflicts3",
  "conflicts4",
  "conflicts5",
  "conflicts6",
  "conflicts7",
  "health_1",
  "health_2",
  "health_3",
  "health_4",
  "health_5",
  "health_6",
  "alcoholic_beverages",
  "violence_parents",
  "tolerance_vaw_1",
  "tolerance_vaw_2",
  "tolerance_vaw_3",
  "tolerance_vaw_4",
  "tolerance_vaw_5",
  "whatsapp_freq",
  "whatsapp_work",
  "he_works",
  "he_studies",
  "work_home",
  "id_sector_1",
  "income_range",
  "sample_pi"
)

blm_covariates_cont <- c(
  "age",
  "years_relationship"
)

blm_covariates_bin <- 
  blm_covariates[
    sapply(
      blm_covariates, 
      function(x) setequal(names(table(blm[[x]])), c("0", "1"))
    )
  ]

blm_covariates_cat <-
  blm_covariates[!blm_covariates %in% c(blm_covariates_cont, blm_covariates_bin)]

# administrative variables that are useful but won't be adjusted for
blm_admin <- c(
  "type_survey",
  "partner",
  "ubigeo",
  "dpto_id",
  "prov_id",
  "dist_id", 
  "id_status",
  "key",
  "start_day",
  "occu",
  "sector_1",
  "id_sector_2",
  "sector_2",
  "id_sector_3",
  "sector_3"
)

# stuff that we can drop
blm_drop <- c(
  "consent",
  "estrangement",
  "tongue_man_spcfy",
  "food_comp",
  "partner_phone_yesno"
)

# add "_m_bl" to men's baseline responses
blm <- blm[, c(id_var, blm_covariates, blm_admin, blm_drop)]

colnames(blm) <- c(
  id_var, 
  paste0(colnames(blm[, c(blm_covariates, blm_admin, blm_drop)]), "_m_bl")
)


blm_covariates <- paste0(blm_covariates, "_m_bl")
blm_covariates_bin <- paste0(blm_covariates_bin, "_m_bl")
blm_covariates_cat <- paste0(blm_covariates_cat, "_m_bl")
blm_covariates_cont <- paste0(blm_covariates_cont, "_m_bl")
blm_admin <- paste0(blm_admin, "_m_bl")
blm_drop <- paste0(blm_drop, "_m_bl")

# sort women's variables

blw_covariates <- c(
  "age",
  "tongue",
  "education",
  "hh_members",
  "hh_members_18y",
  "cohabiting_check1",
  "age_started_cohab",
  "cohabiting_check2",
  "years_cohabiting",
  "decisions_together",
  "control_cooking",
  "control_visiting",
  "control_buying_clothes",
  "ladder1",
  "communication1",
  "communication2",
  "communication3",
  "communication4",
  "communication5",
  "communication6",
  "communication7",
  "communication8",
  "communication9",
  "communication10",
  "communication11",
  "ipv1",
  "ipv1_v2",
  "ipv_childhood",
  "ipv2",
  "ipv3",
  "ipv4",
  "ipv5",
  "ipv6",
  "ipv7",
  "ipv8",
  "ipv9",
  "ipv10",
  "ipv11",
  "ipv12",
  "reaction1",
  "reaction2",
  "reaction3",
  "reaction4",
  "reaction5",
  "alcohol_man",
  "gem1",
  "gem2",
  "gem3",
  "gem4",
  "gem5",
  "gem6",
  "gem6_v2",
  "gem7",
  "gem8",
  "sex_consent",
  "sex_refuse",
  "ask_for_help",
  "she_works",
  "she_studies",
  "id_sector_1",
  "work_home",
  "income_range"
)

blw_covariates_cont <- c(
  "age",
  "hh_members",
  "hh_members_18y",
  "years_cohabiting",
  "age_started_cohab",
  "ladder1"
)

blw_covariates_bin <- 
  blw_covariates[
    sapply(
      blw_covariates, 
      function(x) setequal(names(table(blw[[x]])), c("0", "1"))
    )
  ]

blw_covariates_cat <-
  blw_covariates[!blw_covariates %in% c(blw_covariates_cont, blw_covariates_bin)]

blw_admin <- c(
  "consent",
  "tongue_spcfy",
  "occu",
  "ask_for_help_who",
  "ask_for_help_who_spcfy",
  "ipv_controlb_short",
  "ipv_psycho_short",
  "ipv_physic_short",
  "ipv_sex1_short",
  "ipv_sex2_short",
  "ask_for_help_who_1",
  "ask_for_help_who_2",
  "ask_for_help_who_3",
  "ask_for_help_who_4",
  "ask_for_help_who_5",
  "ask_for_help_who_6",
  "ask_for_help_who_7",
  "ask_for_help_who_8",
  "ask_for_help_who_9",
  "ask_for_help_who_666",
  "ask_for_help_who_999",
  "ask_for_help_who_888",
  "call_status",
  "ubigeo",
  "dpto_id",
  "prov_id",
  "dist_id",
  "start_day",
  "long_short_survey",
  "id_status",
  "sector_1",
  "id_sector_2",
  "sector_2",
  "future_contact",
  "agree_recontact"
)

blw_drop <- c(
  "key"
)

# add "_w_bl" to women's baseline responses
blw <- blw[, c(id_var, blw_covariates, blw_admin, blw_drop)]

colnames(blw) <- c(
  id_var, 
  paste0(colnames(blw[, c(blw_covariates, blw_admin, blw_drop)]), "_w_bl")
)


blw_covariates <- paste0(blw_covariates, "_w_bl")
blw_covariates_bin <- paste0(blw_covariates_bin, "_w_bl")
blw_covariates_cat <- paste0(blw_covariates_cat, "_w_bl")
blw_covariates_cont <- paste0(blw_covariates_cont, "_w_bl")
blw_admin <- paste0(blw_admin, "_w_bl")
blw_drop <- paste0(blw_drop, "_w_bl")


# clean endline -----------------------------------------------------------

# sort men's variables

# covariates - these are things we potentially want to adjust for
elm_covariates <- c(
  "relationship_confirm",
  "breakup",
  "breakup_oth",
  "newpartner",
  "education",
  "control_earnings",
  "control_health_care",
  "control_health_care_oth",
  "control_big_buys",
  "control_big_buys_oth",
  "communication1",
  "communication2",
  "communication3",
  "communication4",
  "satisfaction1",
  "satisfaction2",
  "satisfaction3",
  "satisfaction4",
  "conflicts1",
  "conflicts2",
  "conflicts3",
  "conflicts4",
  "conflicts5",
  "conflicts6",
  "conflicts7",
  "health_1",
  "health_2",
  "health_3",
  "health_4",
  "health_5",
  "health_6",
  "alcoholic_beverages",
  "bias_v1_1",
  "bias_v3_1",
  "bias_v3_2",
  "bias_v3_3",
  "bias_v3_4",
  "bias_v3_5",
  "bias_v3_6",
  "bias_v3_7",
  "bias_v3_8",
  "bias_v3_9",
  "bias_v3_10",
  "bias_v3_11",
  "bias_v3_12",
  "bias_v3_13",
  "tolerance_vaw_1",
  "tolerance_vaw_2",
  "tolerance_vaw_3",
  "tolerance_vaw_4",
  "tolerance_vaw_5",
  "income_change",
  "income_range",
  "work_home"
)

# elm_covariates_cont <- c(
#   "age",
#   "years_relationship"
# )rmc

# administrative variables that are useful but won't be adjusted for
elm_admin <- c(
  "date_start",
  "ubigeo",
  "dpto_id",
  "prov_id",
  "dist_id", 
  "keep_contact",
  "recruitment_1",
  "recruitment_2",
  "recruitment_3",
  "treatment",
  "batch",
  "id_status"
)

elm_impl <- c(
  "rmc_q0",
  "rmc_q0_w",
  "rmc_q0_w_1",
  "rmc_q0_w_2",
  "rmc_q0_w_3",
  "rmc_q0_w_666",
  "rmc_q0_w_777",
  "rmc_q0_w_oth",
  "rmc_1_q1",
  "rmc_1_placebo1",
  "rmc_1_q2",
  "rmc_1_placebo2",
  "rmc_2_placebo1",
  "rmc_2_q1",
  "rmc_2_placebo2",
  "rmc_2_q2",
  "rmc_3_q1",
  "rmc_3_q2",
  "rmc_3_placebo1",
  "rmc_3_placebo2",
  "rmc_4_q1",
  "rmc_4_q2",
  "rmc_4_placebo1",
  "rmc_4_placebo2",
  "rmc_content_share",
  "rmc_content_fol",
  "rmc_content_nofol",
  "rmc_content_nofol_oth",
  "rmc_involvement_freq",
  "dropout_reason",
  "dropout_reason_oth"
)

# stuff that we can drop
# elm_drop <- c(
#   "consent",
#   "estrangement",
#   "tongue_man_spcfy",
#   "food_comp",
#   "partner_phone_yesno"
# )

# add "_m" to men's baseline responses
elm <- elm[, c(id_var, elm_covariates, elm_admin, elm_impl)]

colnames(elm) <- c(
  id_var, 
  paste0(colnames(elm[, c(elm_covariates, elm_admin, elm_impl)]), "_m")
)

elm_covariates <- paste0(elm_covariates, "_m")
elm_admin <- paste0(elm_admin, "_m")
elm_impl <- paste0(elm_impl, "_m")

# sort women's variables

elw_covariates <- c(
  "incentive",
  "relationship_confirm",
  "breakup",
  "breakup_oth",
  "newpartner",
  "education",
  "children_num",
  "children_num5years",
  "pregnant",
  "pregnant_months",
  "control_cooking",
  "control_visiting",
  "control_buying_clothes",
  "control_wear",
  "control_earnings",
  "control_health_care",
  "control_health_care_oth",
  "control_big_buys",
  "control_big_buys_oth",
  "ladder1",
  "communication1",
  "communication2",
  "communication3",
  "communication4",
  "communication5",
  "communication6",
  "communication7",
  "communication8",
  "communication9",
  "communication10",
  "communication11",
  "satisfaction1",
  "satisfaction2",
  "satisfaction3",
  "satisfaction4",
  "reaction1",
  "reaction2",
  "reaction3",
  "reaction4",
  "reaction5",
  "alcohol_man",
  "alcohol",
  "bias_v1",
  "bias_v3_1",
  "bias_v3_2",
  "bias_v3_3",
  "bias_v3_4",
  "bias_v3_5",
  "bias_v3_6",
  "bias_v3_7",
  "bias_v3_8",
  "bias_v3_9",
  "bias_v3_10",
  "bias_v3_11",
  "bias_v3_12",
  "bias_v3_13",
  "gem1",
  "gem2",
  "gem3",
  "gem4",
  "gem5",
  "gem6",
  "gem7",
  "gem8",
  "ipv1",
  "ipv2",
  "ipv3",
  "ipv4",
  "ipv_control_lastime",
  "ipv5",
  "ipv6",
  "ipv_psych_lastime",
  "ipv7",
  "ipv8",
  "ipv9",
  "ipv10",
  "ipv_phys_lastime",
  "ipv11",
  "ipv12",
  "ipv_sex_lastime",
  "sex_consent",
  "sex_refuse",
  "ipv13",
  "ipv14",
  "ipv15",
  "income_change",
  "income_range",
  "income_compare",
  "work_home"
)

elw_covariates_cont <- c(
  "age",
  "hh_members",
  "hh_members_18y",
  "years_relationship",
  "years_cohabiting",
  "age_started_cohab",
  "ladder1"
)

elw_admin <- c(
  "start_date",
  "ubigeo",
  "dpto_id",
  "prov_id",
  "dist_id",
  "long_short_survey",
  "treatment",
  "batch",
  "key",
  "id_status"
)

elw_impl <- c(
  "rmc_aware1",
  "rmc_aware2",
  "rmc_cont_sh",
  "rmc_cont_sh_type",
  "rmc_cont_sh_type_1",
  "rmc_cont_sh_type_2",
  "rmc_cont_sh_type_3",
  "rmc_cont_sh_type_4",
  "rmc_cont_sh_type_777",
  "rmc_cont_sh_type_999",
  "rmc_cont_sh_type_888",
  "rmc_cont_sh_type_oth",
  "rmc_content_fol",
  "rmc_content_nofol",
  "rmc_involvement_freq"
)

# add "_w_bl" to women's baseline responses
elw <- elw[, c(id_var, elw_covariates, elw_admin, elw_impl)]

colnames(elw) <- c(
  id_var, 
  paste0(colnames(elw[, c(elw_covariates, elw_admin, elw_impl)]), "_w")
)

elw_covariates <- paste0(elw_covariates, "_w")
elw_admin <- paste0(elw_admin, "_w")
elw_impl <- paste0(elw_impl, "_w")



