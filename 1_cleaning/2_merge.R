# create merged individual and couple-level datasets 
# 
# total_men <- nrow(blm)
# total_women <- blw$c
  

# merge baseline ----------------------------------------------------------

# merge men's and women's responses
bl <- left_join(blm, blw, by = "participant_id")

# subset to those meeting eligibility for randomization
bl_to_merge <- filter(bl, id_status_w_bl == 1 | agree_recontact_w_bl == 1)

stopifnot(nrow(bl_to_merge) == 2710)


# merge randomization -----------------------------------------------------

# one person was mistakenly in two randomization batches (use only first batch
# as this was the one they were actually assigned to)
rand_to_merge <- filter(rand, !(participant_id == "1773" & batch == "3"))

stopifnot(nrow(rand_to_merge) == 2710)


# another person was randomized to batch 2 group 4, but was physically 
# assigned to batch 2 group 5. Here, we change his group to 5 to reflect
# the group he actually participated in. Because randomization occurred in two 
# stages (first to T/C then to group among T) this does not violate ITT for T 
# vs. C. It is for the second randomization to group. However, we feel that it 
# is important that his group-level values reflect the group he actually 
# participated in as all this happened without his knowledge and appears to be
# completely accidental.
rand_to_merge$group[rand_to_merge$participant_id == 15] <- 10


# merge intervention ------------------------------------------------------

impl_to_merge <- select(impl, -batch, -group)
stopifnot(nrow(impl_to_merge) == 2710 / 2)

messages_to_merge <-
  select(messages,
         participant_id,
         msg_i,
         msg_char_above_one_i,
         msg_word_above_one_i,
         msg_word_above_ten_i,
         msg_comm_emo_reg_i,
         msg_health_sex_i,
         msg_finance_i,
         msg_life_home_i)

messages_nested_to_merge <-
  select(messages_nested,
         participant_id,
         msg_exp,
         msg_char_above_one_exp,
         msg_word_above_one_exp,
         msg_word_above_ten_exp,
         msg_comm_emo_reg_exp,
         msg_health_sex_exp,
         msg_finance_exp,
         msg_life_home_exp)

coded_messages_to_merge <- 
  select(coded_messages,
         participant_id,
         problem_partner_i,
         challenge_beliefs_i,
         participants_argue_i,
         problem_partner_g,
         challenge_beliefs_g,
         participants_argue_g)

recoded_messages_to_merge <- 
  select(recoded_messages,
         participant_id,
         participation_exercise_rev_i,
         program_reinforce_rev_i,
         program_challenge_rev_i,
         problem_partner_detail_rev_i,
         problem_partner_acknowledge_rev_i,
         give_advice_rev_i,
         challenge_beliefs_rev_i,
         fac_challenge_beliefs_rev_i,
         helpful_feedback_i,
         argument_aggresive_rev_i,
         program_engagement_rev_i,
         share_problem_rev_i,
         react_problem_rev_i,
         jpr_incite_conflict_i,
         any_code_rev_i,
         participation_exercise_rev_g,
         program_reinforce_rev_g,
         program_challenge_rev_g,
         problem_partner_detail_rev_g,
         problem_partner_acknowledge_rev_g,
         give_advice_rev_g,
         challenge_beliefs_rev_g,
         fac_challenge_beliefs_rev_g,
         helpful_feedback_g,
         argument_aggresive_rev_g,
         program_engagement_rev_g,
         share_problem_rev_g,
         react_problem_rev_g,
         jpr_incite_conflict_g,
         any_code_rev_g)

recoded_messages_nested_to_merge <- 
  select(recoded_messages_nested,
         participant_id,
         participation_exercise_rev_exp,
         program_reinforce_rev_exp,
         program_challenge_rev_exp,
         problem_partner_detail_rev_exp,
         problem_partner_acknowledge_rev_exp,
         give_advice_rev_exp,
         challenge_beliefs_rev_exp,
         fac_challenge_beliefs_rev_exp,
         argument_aggresive_rev_exp,
         program_engagement_rev_exp,
         share_problem_rev_exp,
         react_problem_rev_exp,
         jpr_incite_conflict_exp,
         any_code_rev_exp,
         participation_exercise_rev_exp,
         program_reinforce_rev_exp,
         program_challenge_rev_exp,
         problem_partner_detail_rev_exp,
         problem_partner_acknowledge_rev_exp,
         give_advice_rev_exp,
         challenge_beliefs_rev_exp,
         fac_challenge_beliefs_rev_exp,
         helpful_feedback_exp,
         argument_aggresive_rev_exp,
         program_engagement_rev_exp,
         share_problem_rev_exp,
         react_problem_rev_exp,
         jpr_incite_conflict_exp,
         any_code_rev_exp)


# merge endline -----------------------------------------------------------

# merge men's and women's responses
el <- left_join(elw, elm, by = "participant_id")

el_to_merge <- select(el, -batch_m, -batch_w)

stopifnot(nrow(el_to_merge) == 2710)


# merge into one analytic dataset
rmc <- 
  rand_to_merge |>
  left_join(el_to_merge, by = "participant_id") |>
  left_join(bl_to_merge, by = "participant_id") |>
  left_join(impl_to_merge, by = "participant_id") |> 
  left_join(messages_to_merge, by = "participant_id") |>
  left_join(messages_nested_to_merge, by = "participant_id") |>
  left_join(coded_messages_to_merge, by = c("participant_id")) |>
  left_join(recoded_messages_to_merge, by = c("participant_id")) |>
  left_join(recoded_messages_nested_to_merge, by = c("participant_id"))


# create useful sample indicators -----------------------------------------

rmc <-
  rmc |>
  mutate(
    el_survey = case_when(
      id_status_m == 1 & id_status_w == 1 ~ "both",
      (id_status_m != 1 | is.na(id_status_m)) & id_status_w == 1 ~ "woman only",
      id_status_m == 1 & id_status_w != 1 ~ "man only",
      (id_status_m != 1 | is.na(id_status_m)) & id_status_w != 1 ~ "neither"
    ),
    bl_survey = case_when(
      id_status_m_bl == 1 & id_status_w_bl == 1 ~ "both",
      (id_status_m_bl != 1 | is.na(id_status_m_bl)) & id_status_w_bl == 1 ~ "woman only",
      id_status_m_bl == 1 & id_status_w_bl != 1 ~ "man only",
      (id_status_m_bl != 1 | is.na(id_status_m_bl)) & id_status_w_bl != 1 ~ "neither"
    )
  )


