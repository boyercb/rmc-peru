

# baseline ----------------------------------------------------------------

# We recruited 3,866 men who completed a baseline survey and shared their
# partner’s cell phone number. We then contacted these women (1) to verify they
# were in fact in a relationship with the man, (2) to ask whether they were okay
# with their partner participating in the program, and (3) to ask them to
# complete a telephone-based baseline survey. Women’s agreement that their male
# partner should participate in the program was essential to us. Therefore, only
# men who’s partners agreed were allowed to enroll in the study. 

# men's baseline
blm <- read_stata(get_data("Baseline/RMC_man_baseline.dta")) 

# women's baseline
blw <- read_stata(get_data("Baseline/RMC_woman_baseline.dta"))

# sanity checks

# check that we have expected number of men
stopifnot(nrow(blm) == 3866)

# check that they are unique
stopifnot(!anyDuplicated(blm$participant_id))

# check that we have expected number of completed women's surveys
stopifnot(sum(blw$id_status == 1) == 2579)

# check that they are unique
stopifnot(!anyDuplicated(blw$participant_id))

# make sure participant id is character vector
blm$participant_id <- as.character(blm$participant_id)
blw$participant_id <- as.character(blw$participant_id)


# randomization -----------------------------------------------------------

# We randomized couples within recruitment batches. As couples completed the
# baseline survey, we formed batches of 500 in which 250 would be randomly
# assigned to receive the HEP program and 250 to the control group.

# read in all 6 randomization batches
batches <-
  map(1:6, function(i)
    read_stata(get_data(
      if (i == 1) {
        paste0("_inputs/batch", i, "_randomization_.dta")
      } else {
        paste0("_inputs/batch", i, "_randomization.dta")
      }
    )))

# combine into single dataset
rand <- bind_rows(batches, .id = 'batch')

# simplify coding of batches and groups
rand <-
  rand |>
  mutate(
    group = if_else(
      batch == 1, b1_group + 1,
      if_else(
        batch == 2, (b2_group + 1) + 5,
        if_else(
          batch == 3, (b3_group + 1) + 10,
          if_else(
            batch == 4, (b4_group + 1) + 15,
            if_else(
              batch == 5, (b5_group + 1) + 20,
              if_else(
                batch == 6, (b6_group + 1) + 25, NA_real_
              )
            )
          )
        )
      )
    ),
    group = replace(group, is.na(group), 0),
    strata = replace(strata, strata == 4 & batch == 1, 3),
    strata = replace(strata, is.na(strata), 4)
  ) |>
  select(participant_id, batch, group, strata, treatment)

# make sure participant id is character vector
rand$participant_id <- as.character(rand$participant_id)


# implementation ----------------------------------------------------------

impl <- read_stata(get_data("Intervention/Intervention_FINAL.dta"))

# recode some of the variables
impl <-
  impl |>
  mutate(
    remained_in_chat = 
      replace(1 - left_group, included == 0 | eliminated == 1, 0),
    days_in_chat = 
      ifelse(
        remained_in_chat == 1,
        30,
        as.Date(left_group_day, "y-m-d") - as.Date(start_day_group, "y-m-d") + 1
      ),
    remained_in_chat_day1 = as.numeric(days_in_chat > 1),
    remained_in_chat_day10 = as.numeric(days_in_chat > 10),
    remained_in_chat_day15 = as.numeric(days_in_chat > 15),
    remained_in_chat_day20 = as.numeric(days_in_chat > 20)
  )

# make sure participant id is character vector
impl$participant_id <- as.character(impl$participant_id)

# read message data
messages <- read_stata(get_data("Intervention/Intervention_raw_all_messages.dta"))
coded_messages <- read_dta(get_data("Intervention/raw_messages_identified.dta"))
recoded_messages <- read_dta(get_data("Intervention/revised_codes_messaged.dta"))
  
# drop facilitator messages 
messages_fac <- filter(messages, type_actor == 2)
messages <- filter(messages, type_actor != 2)

# create day of intervention variable
messages <- 
  messages |>
  group_by(batch, group) |>
  mutate(day = date - first(date) + 1) 

recoded_messages <- 
  recoded_messages |>
  group_by(batch, group) |>
  mutate(day = date - first(date) + 1) 

# drop messages after day 35
messages <- 
  messages |>
  filter(day <= 35)

recoded_messages <- 
  recoded_messages |>
  filter(day <= 35)

# make it any positive coding?
recoded_messages <-
  recoded_messages |>
  mutate(
    across(ends_with("_rev"), \(x) as.numeric(x > 0))
  )

messages_nested <- 
  messages |>
  ungroup() |>
  nest_by(batch, group) 

messages_nested <- 
  left_join(
    select(impl, batch, group, participant_id, days_in_chat, remained_in_chat),
    messages_nested,
    by = c("batch", "group")
  ) 

messages_nested <-
  messages_nested |>
    mutate(
      msg_exp = map2_dbl(data, days_in_chat, \(x, y) sum(x$day <= y, na.rm = TRUE)),
      msg_char_above_one_exp = map2_dbl(data, days_in_chat, \(x, y) sum(x$day <= y & str_length(x$message_complete) > 1, na.rm = TRUE)),
      msg_word_above_one_exp = map2_dbl(data, days_in_chat, \(x, y) sum(x$day <= y & str_count(x$message_complete, '\\w+') > 1, na.rm = TRUE)),
      msg_word_above_ten_exp =  map2_dbl(data, days_in_chat, \(x, y) sum(x$day <= y & str_count(x$message_complete, '\\w+') > 10, na.rm = TRUE)),
      msg_comm_emo_reg_exp = map2_dbl(data, days_in_chat, \(x, y) sum(x$day >= 2 & x$day <= 14 & x$day <= y, na.rm = TRUE)),
      msg_health_sex_exp = map2_dbl(data, days_in_chat, \(x, y) sum(x$day >= 15 & x$day <= 22 & x$day <= y, na.rm = TRUE)),
      msg_finance_exp = map2_dbl(data, days_in_chat, \(x, y) sum(x$day %in% c(23, 24, 27, 28) & x$day <= y, na.rm = TRUE)),
      msg_life_home_exp = map2_dbl(data, days_in_chat, \(x, y) sum(x$day %in% c(25, 26) & x$day <= y, na.rm = TRUE))
    )

recoded_messages_nested <- 
  recoded_messages |>
  ungroup() |>
  nest_by(batch, group)

recoded_messages_nested <- 
  left_join(
    select(impl, batch, group, participant_id, days_in_chat, remained_in_chat),
    recoded_messages_nested,
    by = c("batch", "group")
  ) 

recoded_messages_nested <-
  recoded_messages_nested |>
    mutate(
      participation_exercise_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$participation_exercise_rev, na.rm = TRUE)),
      program_reinforce_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$program_reinforce_rev, na.rm = TRUE)),
      program_challenge_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$program_challenge_rev, na.rm = TRUE)),
      problem_partner_detail_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$problem_partner_detail_rev, na.rm = TRUE)),
      problem_partner_acknowledge_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$problem_partner_acknowledge_rev, na.rm = TRUE)),
      give_advice_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$give_advice_rev, na.rm = TRUE)),
      challenge_beliefs_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$challenge_beliefs_rev, na.rm = TRUE)),
      fac_challenge_beliefs_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$fac_challenge_beliefs_rev, na.rm = TRUE)),
      argument_aggresive_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$argument_aggresive_rev, na.rm = TRUE)),
      program_engagement_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$program_engagement_rev, na.rm = TRUE)),
      share_problem_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$share_problem_rev, na.rm = TRUE)),
      react_problem_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$react_problem_rev, na.rm = TRUE)),
      any_code_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$any_code_rev, na.rm = TRUE)),
      jpr_incite_conflict_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * x$jpr_incite_conflict, na.rm = TRUE)),
      react_problem_rev_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * (x$react_problem_rev - x$jpr_incite_conflict), na.rm = TRUE)), 
      helpful_feedback_exp = map2_dbl(data, days_in_chat, \(x, y) sum((x$day <= y) * (x$give_advice_rev + x$challenge_beliefs_rev + x$fac_challenge_beliefs_rev), na.rm = TRUE))
    )

# messages <-
#   messages |>
#   filter(str_length(message_complete) > 1)



# calculate number of group messages per day and individual messages per day
messages <- 
  messages |>
  group_by(batch, group, participant_id) |>
  summarise(
    msg_i = n(),
    msg_char_above_one_i = sum(str_length(message_complete) > 1),
    msg_word_above_one_i = sum(str_count(message_complete, '\\w+') > 1),
    msg_word_above_ten_i = sum(str_count(message_complete, '\\w+') > 10),
    msg_comm_emo_reg_i = sum(day >= 2 & day <= 14),
    msg_health_sex_i = sum(day >= 15 & day <= 22),
    msg_finance_i = sum(day %in% c(23, 24, 27, 28)),
    msg_life_home_i = sum(day %in% c(25, 26)),
    .groups = "drop"
  ) |>
  right_join(
    select(impl, batch, group, participant_id),
    by = c("batch", "group", "participant_id")
  ) |>
  mutate(
    across(starts_with("msg_"), ~replace_na(.x, 0))
  )


coded_messages <- 
  coded_messages |>
  group_by(batch, group, type_actor, participant_id) |>
  summarise(
    problem_partner_i = sum(problem_partner, na.rm = TRUE),
    challenge_beliefs_i = sum(challenge_beliefs, na.rm = TRUE),
    participants_argue_i = sum(participants_argue, na.rm = TRUE),
    .groups = "drop"
  ) |>
  full_join(
    select(impl, batch, group, participant_id),
    by = c("batch", "group", "participant_id")
  ) |>
  mutate(
    across(ends_with("_i"), ~replace_na(.x, 0))
  ) |>
  group_by(batch, group) |>
  mutate(
    problem_partner_g = sum(problem_partner_i, na.rm = TRUE),
    challenge_beliefs_g = sum(challenge_beliefs_i, na.rm = TRUE),
    participants_argue_g = sum(participants_argue_i, na.rm = TRUE)
  ) |>
  mutate(
    original_group = group,
    group = (batch - 1) * 5 + group,
    batch = as.character(batch)
  ) |>
  ungroup() 
  #filter(type_actor != "Facilitator")

recoded_messages <- 
  recoded_messages |>
  group_by(batch, group, type_actor, participant_id) |>
  summarise(
    participation_exercise_rev_i = sum(participation_exercise_rev, na.rm = TRUE),
    program_reinforce_rev_i = sum(program_reinforce_rev, na.rm = TRUE),
    program_challenge_rev_i = sum(program_challenge_rev, na.rm = TRUE),
    problem_partner_detail_rev_i = sum(problem_partner_detail_rev, na.rm = TRUE),
    problem_partner_acknowledge_rev_i = sum(problem_partner_acknowledge_rev, na.rm = TRUE),
    give_advice_rev_i = sum(give_advice_rev, na.rm = TRUE),
    challenge_beliefs_rev_i = sum(challenge_beliefs_rev, na.rm = TRUE),
    fac_challenge_beliefs_rev_i = sum(fac_challenge_beliefs_rev, na.rm = TRUE),
    argument_aggresive_rev_i = sum(argument_aggresive_rev + jpr_incite_conflict, na.rm = TRUE),
    program_engagement_rev_i = sum(program_engagement_rev, na.rm = TRUE),
    share_problem_rev_i = sum(share_problem_rev, na.rm = TRUE),
    react_problem_rev_i = sum(react_problem_rev, na.rm = TRUE),
    any_code_rev_i = sum(any_code_rev, na.rm = TRUE),
    jpr_incite_conflict_i = sum(jpr_incite_conflict, na.rm = TRUE),
    react_problem_rev_i = react_problem_rev_i - jpr_incite_conflict_i,
    helpful_feedback_i = give_advice_rev_i + challenge_beliefs_rev_i + fac_challenge_beliefs_rev_i,
    .groups = "drop"
  ) |>
  full_join(
    select(impl, batch, group, participant_id),
    by = c("batch", "group", "participant_id")
  ) |>
  mutate(
    across(ends_with("_i"), ~replace_na(.x, 0)),
    type_actor = replace_na(type_actor, "Participant")
  ) |>
  group_by(batch, group) |>
  mutate(
    participation_exercise_rev_g = sum(participation_exercise_rev_i, na.rm = TRUE)  - participation_exercise_rev_i,
    program_reinforce_rev_g = sum(program_reinforce_rev_i, na.rm = TRUE) - program_reinforce_rev_i,
    program_challenge_rev_g = sum(program_challenge_rev_i, na.rm = TRUE) - program_challenge_rev_i,
    problem_partner_detail_rev_g = sum(problem_partner_detail_rev_i, na.rm = TRUE) - problem_partner_detail_rev_i,
    problem_partner_acknowledge_rev_g = sum(problem_partner_acknowledge_rev_i, na.rm = TRUE) - problem_partner_acknowledge_rev_i,
    give_advice_rev_g = sum(give_advice_rev_i, na.rm = TRUE) - give_advice_rev_i,
    challenge_beliefs_rev_g = sum(challenge_beliefs_rev_i, na.rm = TRUE) - challenge_beliefs_rev_i,
    fac_challenge_beliefs_rev_g = sum(fac_challenge_beliefs_rev_i, na.rm = TRUE) - fac_challenge_beliefs_rev_i,
    argument_aggresive_rev_g = sum(argument_aggresive_rev_i, na.rm = TRUE) - argument_aggresive_rev_i,
    program_engagement_rev_g = sum(program_engagement_rev_i, na.rm = TRUE) - program_engagement_rev_i,
    share_problem_rev_g = sum(share_problem_rev_i, na.rm = TRUE) - share_problem_rev_i,
    react_problem_rev_g = sum(react_problem_rev_i, na.rm = TRUE) - react_problem_rev_i,
    any_code_rev_g = sum(any_code_rev_i, na.rm = TRUE) - any_code_rev_i,
    jpr_incite_conflict_g = sum(jpr_incite_conflict_i, na.rm = TRUE) - jpr_incite_conflict_i,
    helpful_feedback_g = sum(helpful_feedback_i, na.rm = TRUE) - helpful_feedback_i
    # react_problem_rev_g = react_problem_rev_g - jpr_incite_conflict_g,
    # helpful_feedback_g = give_advice_rev_g + challenge_beliefs_rev_g + fac_challenge_beliefs_rev_g
  ) |>
  mutate(
    original_group = group,
    group = (batch - 1) * 5 + group,
    batch = as.character(batch)
  ) |>
  ungroup() |>
  filter(type_actor != "Facilitator")


# endline -----------------------------------------------------------------

# men's endline
elm <- read_stata(get_data("Endline/RMC_man_endline.dta")) 

# women's endline
elw <- read_stata(get_data("Endline/RMC_woman_endline.dta"))

# make sure participant id is character vector
elm$participant_id <- as.character(elm$participant_id)
elw$participant_id <- as.character(elw$participant_id)

# check that they are unique
stopifnot(!anyDuplicated(elm$participant_id))

# check that they are unique
stopifnot(!anyDuplicated(elw$participant_id))

