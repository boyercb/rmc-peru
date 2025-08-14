df <- 
  y_selected |>
  select(outcome, covariate) |> 
  nest_by(outcome) |> 
  mutate(data = map(data, ~as.vector(.x)), 
         outcome = case_when(
           outcome == "ipv11_w" ~ "any_forced_sex",
           outcome == "ipv12_w" ~ "any_forced_other",
           TRUE ~ outcome
         ))

lasso_selections <- df |> pull(data)
names(lasso_selections) <- df$outcome

df <- 
  y_selected_mech |>
  select(outcome, covariate) |> 
  nest_by(outcome) |> 
  mutate(data = map(data, ~as.vector(.x)), 
         outcome = case_when(
           outcome == "ipv11_w" ~ "any_forced_sex",
           outcome == "ipv12_w" ~ "any_forced_other",
           TRUE ~ outcome
         ))

lasso_selections_mech <- df |> pull(data)
names(lasso_selections_mech) <- df$outcome


df <- 
  y_selected_strata |>
  filter(strata == 2) |>
  select(outcome, covariate) |> 
  nest_by(outcome) |> 
  mutate(data = map(data, ~as.vector(.x)), 
         outcome = case_when(
           outcome == "ipv11_w" ~ "any_forced_sex",
           outcome == "ipv12_w" ~ "any_forced_other",
           TRUE ~ outcome
         ))

lasso_selections_any_ipv_bl <- df |> pull(data)
names(lasso_selections_any_ipv_bl) <- df$outcome


df <- 
  y_selected_strata_att |>
  filter(strata == 1) |>
  select(outcome, covariate) |> 
  nest_by(outcome) |> 
  mutate(data = map(data, ~as.vector(.x)), 
         outcome = case_when(
           outcome == "ipv11_w" ~ "any_forced_sex",
           outcome == "ipv12_w" ~ "any_forced_other",
           TRUE ~ outcome
         ))

lasso_selections_tolerance_vaw_any_bl <- df |> pull(data)
names(lasso_selections_tolerance_vaw_any_bl) <- df$outcome


df <- 
  y_selected_strata_att |>
  filter(strata == 0) |>
  select(outcome, covariate) |> 
  nest_by(outcome) |> 
  mutate(data = map(data, ~as.vector(.x)), 
         outcome = case_when(
           outcome == "ipv11_w" ~ "any_forced_sex",
           outcome == "ipv12_w" ~ "any_forced_other",
           TRUE ~ outcome
         ))

lasso_selections_tolerance_vaw_none_bl <- df |> pull(data)
names(lasso_selections_tolerance_vaw_none_bl) <- df$outcome

df <- 
  y_selected_strata_mech_att |>
  filter(strata == 1) |>
  select(outcome, covariate) |> 
  nest_by(outcome) |> 
  mutate(data = map(data, ~as.vector(.x)), 
         outcome = case_when(
           outcome == "ipv11_w" ~ "any_forced_sex",
           outcome == "ipv12_w" ~ "any_forced_other",
           TRUE ~ outcome
         ))

lasso_selections_mech_tolerance_vaw_any_bl <- df |> pull(data)
names(lasso_selections_mech_tolerance_vaw_any_bl) <- df$outcome


df <- 
  y_selected_strata_mech_att |>
  filter(strata == 0) |>
  select(outcome, covariate) |> 
  nest_by(outcome) |> 
  mutate(data = map(data, ~as.vector(.x)), 
         outcome = case_when(
           outcome == "ipv11_w" ~ "any_forced_sex",
           outcome == "ipv12_w" ~ "any_forced_other",
           TRUE ~ outcome
         ))

lasso_selections_mech_tolerance_vaw_none_bl <- df |> pull(data)
names(lasso_selections_mech_tolerance_vaw_none_bl) <- df$outcome


