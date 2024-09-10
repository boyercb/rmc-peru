# create merged individual and couple-level datasets 
# 
# total_men <- nrow(blm)
# total_women <- blw$c
  
# make sure participant id is character vector
blm$participant_id <- as.character(blm$participant_id)
blw$participant_id <- as.character(blw$participant_id)
impl$participant_id <- as.character(impl$participant_id)
rand$participant_id <- as.character(rand$participant_id)
elm$participant_id <- as.character(elm$participant_id)
elw$participant_id <- as.character(elw$participant_id)


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


# merge intervention ------------------------------------------------------

impl_to_merge <- select(impl, -batch, -group)
stopifnot(nrow(impl_to_merge) == 2710 / 2)

messages_to_merge <-
  select(messages,
         participant_id,
         msg_i,
         msg_comm_emo_reg_i,
         msg_health_sex_i,
         msg_finance_i,
         msg_life_home_i)

coded_messages_to_merge <- 
  select(coded_messages,
         participant_id,
         problem_partner_i,
         challenge_beliefs_i,
         participants_argue_i,
         problem_partner_g,
         challenge_beliefs_g,
         participants_argue_g)


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
  left_join(coded_messages_to_merge, by = c("participant_id"))

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


