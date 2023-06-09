
# primary outcomes --------------------------------------------------------

# Physical and sexual intimate partner violence - a dummy variable that takes
# the value of 1 if a woman has been a victim of at least one form of physical
# or sexual intimate partner violence in the last 6 months (M10-7, M10-8, M10-9,
# M10-10, M10-11, M10-12). Takes the value of 0 otherwise.

rmc <- 
  rmc |>
  mutate(
    any_control = case_when(
      ipv1_w > 1 | ipv2_w > 1 | ipv3_w > 1 | ipv4_w > 1 ~ 1,
      ipv1_w == 1 & ipv2_w == 1 & ipv3_w == 1 & ipv4_w == 1 ~ 0,
      TRUE ~ NA_integer_
    ),
    any_psychological = case_when(
      ipv5_w > 1 | ipv6_w > 1 ~ 1,
      ipv5_w == 1 & ipv6_w == 1 ~ 0,
      TRUE ~ NA_integer_
    ),
    any_ipv = case_when(
      ipv7_w > 1 | ipv8_w > 1 | ipv9_w > 1 | ipv10_w > 1 | ipv11_w > 1 | ipv12_w > 1 ~ 1,
      ipv7_w == 1 & ipv8_w == 1 & ipv9_w == 1 & ipv10_w == 1 & ipv11_w == 1 & ipv12_w == 1 ~ 0,
      TRUE ~ NA_integer_
    ),
    any_physical = case_when(
      ipv7_w > 1 | ipv8_w > 1 | ipv9_w > 1 | ipv10_w > 1 ~ 1,
      ipv7_w == 1 & ipv8_w == 1 & ipv9_w == 1 & ipv10_w == 1 ~ 0,
      TRUE ~ NA_integer_
    ),
    any_sexual = case_when(
      ipv11_w > 1 | ipv12_w > 1 ~ 1,
      ipv11_w == 1 & ipv12_w == 1 ~ 0,
      TRUE ~ NA_integer_
    ),
    any_cyber = case_when(
      ipv13_w > 1 | ipv14_w > 1 | ipv15_w > 1 ~ 1,
      ipv13_w == 1 & ipv14_w == 1 & ipv15_w == 1 ~ 0,
      TRUE ~ NA_integer_
    )
    
  )

rmc <- 
  rmc |>
  rowwise() |>
  mutate(
    control_score = mean(c(ipv1_w, ipv2_w, ipv3_w, ipv4_w), na.rm = TRUE),
    control_nonmiss = sum(!is.na(c(ipv1_w, ipv2_w, ipv3_w, ipv4_w))),
    
    psychological_score = mean(c(ipv5_w, ipv6_w), na.rm = TRUE),
    psychological_nonmiss = sum(!is.na(c(ipv5_w, ipv6_w))),

    ipv_score = mean(c(ipv7_w, ipv8_w, ipv9_w, ipv10_w, ipv11_w, ipv12_w), na.rm = TRUE),
    ipv_nonmiss = sum(!is.na(c(ipv7_w, ipv8_w, ipv9_w, ipv10_w, ipv11_w, ipv12_w))),
    
    physical_score = mean(c(ipv7_w, ipv8_w, ipv9_w, ipv10_w), na.rm = TRUE),
    physical_nonmiss = sum(!is.na(c(ipv7_w, ipv8_w, ipv9_w, ipv10_w))),
    
    sexual_score = mean(c(ipv11_w, ipv12_w), na.rm = TRUE),
    sexual_nonmiss = sum(!is.na(c(ipv11_w, ipv12_w))),
    
    cyber_score = mean(c(ipv13_w, ipv14_w, ipv15_w), na.rm = TRUE),
    cyber_nonmiss = sum(!is.na(c(ipv13_w, ipv14_w, ipv15_w))),
    
    control_score = (control_score - 1) / 4,
    psychological_score = (psychological_score - 1) / 4,
    ipv_score = (ipv_score - 1) / 4,
    physical_score = (physical_score - 1) / 4,
    sexual_score = (sexual_score - 1) / 4,
    cyber_score = (cyber_score - 1) / 4
  ) |>
  ungroup()

# time to event
rmc <- 
  rmc |>
  mutate(
    
    across(
      c(ipv_control_lastime_w, 
        ipv_psych_lastime_w,
        ipv_phys_lastime_w,
        ipv_sex_lastime_w),
      ~replace(.x, is.na(.x) & id_status_w == 1, 7)
    )
    
  )


# Control and decision-making - is a simple arithmetic mean index of 8 questions
# from the women's responses (M3-1i, M3-1ii, M3-1iii, M3-1iv, M3-2, M3-3, M3-4,
# M3-5). Index takes values 0 to 1, when 0 signifies the worst possible outcome
# and 1 the best.
    
rmc <-
  rmc |>
  mutate(
    across(
      all_of(control_items[1:4]), 
      function(x) (4 - x) / 3
    ),
    across(
      all_of(control_items[5:7]), 
      function(x) as.numeric(replace(x %in% c(1, 3, 4), is.na(x), NA))
    ),
    control_index = 
      (control_cooking_w + control_visiting_w +
         control_buying_clothes_w + control_wear_w +
         control_earnings_w + control_health_care_w +
         control_big_buys_w) / 7
  )


# Sexual consent - is a simple arithmetic mean index of 2 questions from the
# women's responses (M10-sex-dec, M10-sex-ref). Index takes values 0 to 1, when
# 0 signifies the worst possible outcome and 1 the best.

rmc <-
  rmc |>
  mutate(
    sex_consent_w = (4 - sex_consent_w) / 3,
    sex_refuse_w = (sex_refuse_w - 1) / 3,
    consent_index = (sex_consent_w + sex_refuse_w) / 2
  )


# Communication and conflict resolution - is a simple arithmetic mean index
# compounding 11 questions for men (H3-1i, H3-1ii, H3-1iii, H3-1iv, H5-1i,
# H5-1ii, H5-1iii, H5-1iv, H5-1v, H5-1vi, H5-1vii), and 16 questions for women
# (M4-1i, M4-1ii, M4-1iii, M4-1iv; M4-2i, M4-2ii, M4-2iii, M4-2iv, M4-2v,
# M4-2vi, M4-2vii;  M6-1i, M6-1ii, M6-1iii, M6-1iv, M6-1v). In addition, three
# separate indices will be constructed from women's responses: an index of
# positive communication at home (M4-1i, M4-1ii, M4-1iii, M4-1iv, M4-2ii,
# M4-2iii, M4-2iv, M4-2vi, M4-2vii), an index of negative communication at home
# (M4-2i, M4-2v), and an index of positive conflict resolution skills (M6-1i,
# M6-1ii, M6-1iii, M6-1iv, M6-1v). Indices takes values 0 to 1, when 0 signifies
# the worst possible outcome and 1 the best.

rmc <-
  rmc |>
  mutate(
    never_argue_m = as.numeric((conflicts1_m + conflicts2_m + conflicts3_m + 
      conflicts4_m + conflicts5_m + conflicts6_m + conflicts7_m) == 0),
    
    across(
      all_of(paste0("communication", c(5, 9), "_w")),
      function(x) 4 - x
    ),
    
    across(
      all_of(paste0("communication", 1:11, "_w")),
      function(x) x / 4
    ),
    
    across(
      all_of(paste0("communication", 1:4, "_m")),
      function(x) x / 4
    ),
    
    across(
      all_of(paste0("reaction", c(2, 3), "_w")),
      function(x) x >= 3
    ),
    across(
      all_of(paste0("reaction", c(1, 5), "_w")),
      function(x) x == 1
    ),
    # across(
    #   all_of(paste0("reaction", 1:5, "_w")), 
    #   ~replace(.x, never_argue_m == 1, 4)
    # ),
    comm_index = (communication1_w + communication2_w +
      communication3_w + communication4_w +
      communication5_w + communication6_w + 
      communication7_w + communication8_w +
      communication9_w + communication10_w +
      communication11_w  + communication1_m + communication2_m +
      communication3_m + communication4_m + reaction1_w +
      reaction2_w + reaction3_w + reaction5_w) / 19
  )


# secondary outcomes ------------------------------------------------------

# Men's mental health - a simple arithmetic mean index of 6 questions from the
# men’s responses (H6-1i, H6-1ii, H6-1iii, H6-1iv, H6-1v, H6-1vi). Index takes
# values 0 to 1, when 0 signifies the worst possible outcome and 1 the best.
  

rmc <- 
  rmc |>
  mutate(
    depression_m = (health_1_m + health_2_m + health_3_m +
      health_4_m + health_5_m + health_6_m) / 30
  )

# Relationship satisfaction - a simple arithmetic mean index of 4 questions from
# the women’s responses (M5-1a, M5-1b, M5-2a, M5-2b). Index takes values 0 to 1,
# when 0 signifies the worst possible outcome and 1 the best. We will analyze
# this separately for women and men as well as jointly.


rmc <- 
  rmc |>
  mutate(
    across(
      all_of(satisfaction_items), 
      function(x) 6 - x
    ),
    satisfaction_m = (satisfaction1_m + satisfaction3_m) / 12,
    satisfaction_m_perc_w = (satisfaction2_m + satisfaction4_m) / 12,
    satisfaction_w = (satisfaction1_w + satisfaction3_w) / 12,
    satisfaction_w_perc_m = (satisfaction2_w + satisfaction4_w) / 12
  )

# Relationship dissolution - an indicator whether (at the moment of the endline)
# the couple remains in a relationship or broke up based on the woman's
# response. The indicator takes the value of 0 if broke up and 1 if they still a
# couple.

rmc <- 
  rmc |>
  mutate(
    brokeup = 1 - relationship_confirm_w
  )

# Men's attitudes towards violence/gender - a simple arithmetic mean index
# of 5 questions from the men’s responses (H8-1i, H8-1ii, H8-1iii, H8-1iv,
# H8-1v, H8-1vi). Index takes values 0 to 1, when 0 signifies the worst possible
# outcome and 1 the best.

# Women's attitudes towards violence/gender - a simple arithmetic mean index of
# 8 questions from the women’s responses (M9-1i, M9-1ii, M9-1iii, M9-1iv, M9-1v,
# M9-1vi, M9-1vii, M9-1viii). Index takes values 0 to 1, when 0 signifies the
# worst possible outcome and 1 the best.



 rmc <- 
  rmc |>
  mutate(
    attitudes_w = (32 - (gem1_w + gem2_w + gem3_w + gem4_w + gem5_w + gem6_w + 
                           gem7_w + gem8_w)) / 32, 
    attitudes_m = (20 - (tolerance_vaw_1_m + tolerance_vaw_2_m + tolerance_vaw_3_m +
                           tolerance_vaw_4_m + tolerance_vaw_5_m)) / 20
  )


rmc <-
  rmc |>
  mutate(
    arguments = (conflicts1_m + conflicts2_m + conflicts3_m +
                   conflicts4_m + conflicts5_m + conflicts6_m +
                   conflicts7_m) / 28
  )


rmc <-
  rmc |>
  mutate(
    ladder1_w = ladder1_w / 10
  )


# experimenter demand -----------------------------------------------------

rmc <- 
  rmc |>
  mutate(
    bias_v1_w = bias_v1_w,
    bias_v1_m = bias_v1_1_m,
    
    across(all_of(paste0("bias_v3_", c(1:4, 6, 8, 11:12), "_w")), function(x) 1 - x),
    marlowe_w = (bias_v3_1_w + bias_v3_2_w + bias_v3_3_w + bias_v3_4_w +
                 bias_v3_5_w + bias_v3_6_w + bias_v3_7_w + bias_v3_8_w + 
                 bias_v3_9_w + bias_v3_10_w + bias_v3_11_w + bias_v3_12_w + 
                 bias_v3_13_w) / 13,
    
    across(all_of(paste0("bias_v3_", c(1:4, 6, 8, 11:12), "_m")), function(x) 1 - x),
    marlowe_m = (bias_v3_1_m + bias_v3_2_m + bias_v3_3_m + bias_v3_4_m +
                   bias_v3_5_m + bias_v3_6_m + bias_v3_7_m + bias_v3_8_m + 
                   bias_v3_9_m + bias_v3_10_m + bias_v3_11_m + bias_v3_12_m + 
                   bias_v3_13_m) / 13
    
  )


