

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

# read message data
messages <- read_stata(get_data("Intervention/Intervention_raw_all_messages.dta"))
coded_messages <- read_dta(get_data("Intervention/raw_messages_identified.dta"))

# drop facilitator messages 
messages_fac <- filter(messages, type_actor == 2)
messages <- filter(messages, type_actor != 2)

# create day of intervention variable
messages <- 
  messages |>
  group_by(batch, group) |>
  mutate(day = date - first(date) + 1) 

# drop messages after day 35
messages <- 
  messages |>
  filter(day <= 35)

# messages <-
#   messages |>
#   filter(str_length(message_complete) > 1)

# calculate number of group messages per day and individual messages per day
messages <- 
  messages |>
  group_by(batch, group, participant_id) |>
  summarise(
    msg_i = n(),
    msg_comm_emo_reg_i = sum(day >= 2 & day <= 14),
    msg_health_sex_i = sum(day >= 15 & day <= 22),
    msg_finance_i = sum(day %in% c(23, 24, 27, 28)),
    msg_life_home_i = sum(day %in% c(25, 26)),
    .groups = "drop"
  ) 

coded_messages <- 
  coded_messages |>
  group_by(batch, group, participant_id) |>
  summarise(
    problem_partner_i = sum(problem_partner, na.rm = TRUE),
    challenge_beliefs_i = sum(challenge_beliefs, na.rm = TRUE),
    participants_argue_i = sum(participants_argue, na.rm = TRUE),
    .groups = "drop"
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


# endline -----------------------------------------------------------------

# men's endline
elm <- read_stata(get_data("Endline/RMC_man_endline.dta")) 

# women's endline
elw <- read_stata(get_data("Endline/RMC_woman_endline.dta"))


# check that they are unique
stopifnot(!anyDuplicated(elm$participant_id))

# check that they are unique
stopifnot(!anyDuplicated(elw$participant_id))

