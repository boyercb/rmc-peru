

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
      paste0("_inputs/batch", i, "_randomization.dta")
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
    strata = replace(strata, is.na(strata) & batch == 1, 4),
    strata = replace(strata, is.na(strata), 5)
  ) |>
  select(participant_id, batch, group, strata, treatment)

  
# implementation ----------------------------------------------------------

impl <- read_stata(get_data("Intervention/Intervention_FINAL.dta"))

# recode some of the variables
impl <-
  impl |>
  mutate(
    remained_in_chat = 
      replace(1 - left_group, included == 0 | eliminated == 1, 0)
  )


# endline -----------------------------------------------------------------

# men's endline
elm <- read_stata(get_data("Endline/RMC_man_endline.dta")) 

# women's endline
elw <- read_stata(get_data("Endline/RMC_woman_endline.dta"))


# check that they are unique
stopifnot(!anyDuplicated(elm$participant_id))

# check that they are unique
stopifnot(!anyDuplicated(elw$participant_id))

