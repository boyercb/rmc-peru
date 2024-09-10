balance_covariates_men <- c(
  "Man's age" = "age_m_bl",
  "Man has some post-secondary education" =  "I(education_w_bl >= 5)",
  "Years together (m)" = "years_relationship_m_bl",
  "Man is employed" = "he_works_m_bl",
  "Man uses WhatsApp daily" = "I(whatsapp_freq_m_bl == 1)",
  "Man was recruited via social media" =  "I(sample_pi_m_bl == 1)",
  "Conflict index (m)" = "arguments_m_bl",
  "Any justification of violence (m)" = "tolerance_vaw_any_bl",
  "Justification of violence index (m)" = "tolerance_vaw_index_bl",
  "Man's father was violent" = "I(violence_parents_m_bl > 1)"
)

balance_covariates_women <- c(
  "Woman's age" = "age_w_bl",
  # "years_cohabiting_w_bl",
  "Woman has some post-secondary education" = "I(education_w_bl >= 5)",
  "Household size" = "hh_members_w_bl",
  "Woman is employed" = "she_works_w_bl",
  "Man drinks alcohol (w)" = "I(alcohol_man_w_bl > 1)",
  "Decision-making power (w)" = "ladder1_w_bl",
  "Control index (w)" = "control_index_bl",
  "Communication index (w)" = "comm_w_bl",
  "Perceived control over sex (w)" = "consent_index_bl",
  "Men's ability to self-regulate (w)" = "emo_reg_w_bl",
  "Any justification of violence (w)" = "tolerance_vaw_any_w_bl",
  "Justification of violence index (w)" = "tolerance_vaw_index_w_bl",
  "Any psychological violence (w)" = "any_psychological_bl",
  "Any physical violence (w)" ="any_physical_bl",
  "Any sexual violence (w)" = "any_sexual_bl"
)

balance_covariates <-
  c(balance_covariates_women[1],
    balance_covariates_men[1], 
    balance_covariates_women[2],
    balance_covariates_men[2], 
    balance_covariates_men[3],
    balance_covariates_women[3],
    balance_covariates_women[4],
    balance_covariates_men[4],
    balance_covariates_men[5],
    balance_covariates_men[6],
    balance_covariates_women[5:10],
    balance_covariates_men[7],
    balance_covariates_women[11],
    balance_covariates_women[12],
    balance_covariates_men[8],
    balance_covariates_men[9],
    balance_covariates_women[13:15],
    balance_covariates_men[10])

balance_regs <-
  lapply(balance_covariates,
         function(x) {
           fits <- list(
             lm_robust(
               formula = reformulate(termlabels = "1",
                                     response = x),
               data = if (x %in% balance_covariates_women) {
                 subset(rmc, treatment == 0 & batch != 6)
               } else {
                 subset(rmc, treatment == 0)
               }
             ) |> tidy(),
             lm_robust(
               formula = reformulate(termlabels = "1",
                                     response = x),
               data = if (x %in% balance_covariates_women) {
                 subset(rmc, treatment == 1 & batch != 6)
               } else {
                 subset(rmc, treatment == 1)
               }
             ) |> tidy(),
             lm_robust(
               formula = reformulate(
                 termlabels = if (x %in% balance_covariates_women) {
                   if (x %in% c("any_physical_bl", "any_sexual_bl")) {
                     c(
                       "treatment",
                       paste0("strata_new_", 3, "_c"),
                       paste0("batch_", 2:5, "_c")
                     )
                   } else {
                     c(
                       "treatment",
                       paste0("strata_new_", 2:3, "_c"),
                       paste0("batch_", 2:5, "_c")
                     )
                   }
                   
                 } else {
                   c(
                     "treatment",
                     paste0("strata_new_", 2:4, "_c"),
                     paste0("batch_", 2:5, "_c")
                   )
                 },
                 response = x
               ),
               data = if (x %in% balance_covariates_women) {
                 subset(rmc, batch != 6)
               } else {
                 rmc
               }
             ) |> tidy()
           )
           
           bind_rows(fits, .id = "id")
         })

balance_regs <- bind_rows(balance_regs)

balance_table <- 
  balance_regs |>
  filter(!(term == "(Intercept)" & id == 3)) |>
  filter(!term %in% c(paste0("strata_new_", 2:4, "_c"),
                     paste0("batch_", 2:5, "_c"))) |>
  select(id, estimate, std.error, df, outcome) |>
  pivot_longer(c(estimate, std.error)) |>
  mutate(
    id = case_when(
      id == 1 ~ "control", 
      id == 2 ~ "treatment",
      id == 3 ~ "diff"
    ),
    value = ifelse(name == "estimate",
                   ifelse(id == "diff", as.character(specd(value, 3)), as.character(specd(value, 3))),
                   paste0("(", specd(value, 3), ")")),
    df = replace(df, name == "std.error", NA),
    outcome = rep(names(balance_covariates), each = 6)
    ) |>
  pivot_wider(names_from = id, values_from = c(df, value)) |> 
  mutate(outcome = replace(outcome, name == "std.error", NA)) |>
  select(outcome, df_treatment, value_treatment, df_control, value_control, value_diff) 

# run the joint test
fit <- lm_robust(
  formula = reformulate(
    termlabels = balance_covariates,
    response = "treatment"
  ),
  data = rmc
)

fstat <- fit$fstatistic
fpval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

add_row <- tibble(
  outcome = c(paste0("Joint F-statistic (df1=", round(fstat[2]), ", df2=", round(fstat[3]), ")"),  "P-value"),
  value_diff = c(specd(fstat[1], 3), specd(fpval, 3))
)

balance_table <- bind_rows(balance_table, add_row)

options(knitr.kable.NA = '')


kable(
  x = balance_table,
  format = "latex",
  col.names = c(" ", "N", "{Mean/SE}", "N", "{Mean/SE}", "{Difference}"),
  align = c("l", "c", "d", "c" , "d", "d"),
  booktabs = TRUE,
  linesep = "",
  caption = "Baseline characteristics, all randomized couples \\label{tab:balance}",
  escape = FALSE
) |>
  kable_styling(font_size = 10) |>
  row_spec(
    row = nrow(balance_table) - 2,
    hline_after = TRUE
  ) |>
  add_header_above(c(
    " " = 1,
    "HEP" = 2,
    "Control" = 2,
    " " = 1
  )) |> 
  save_kable("HEP-manuscript/tables/balance.tex")

recruit_covariates <- c(balance_covariates_women[11],
                        balance_covariates_women[12],
                        balance_covariates_men[8],
                        balance_covariates_men[9],
                        balance_covariates_men[7],
                        balance_covariates_women[13:15])
recruit_regs <-
  lapply(recruit_covariates,
       function(x) {
         fits <- list(
           lm_robust(
             formula = reformulate(termlabels = "1",
                                   response = x),
             data = if (x %in% balance_covariates_women) {
               subset(rmc, sample_pi_m_bl == 1 & batch != 6)
             } else {
               subset(rmc, sample_pi_m_bl == 1)
             }
           ) |> tidy(),
           lm_robust(
             formula = reformulate(termlabels = "1",
                                   response = x),
             data = if (x %in% balance_covariates_women) {
               subset(rmc, sample_pi_m_bl == 2 & batch != 6)
             } else {
               subset(rmc, sample_pi_m_bl == 2)
             }
           ) |> tidy(),
           lm_robust(
             formula = reformulate(termlabels = "1",
                                   response = x),
             data = if (x %in% balance_covariates_women) {
               subset(rmc, sample_pi_m_bl == 3 & batch != 6)
             } else {
               subset(rmc, sample_pi_m_bl %in% c(3, 4))
             }
           ) |> tidy()
           # lm_robust(
           #   formula = reformulate(
           #     termlabels = if (x %in% balance_covariates_women) {
           #       c(
           #         "treatment",
           #         paste0("strata_new_", 2:3, "_c"),
           #         paste0("batch_", 2:5, "_c")
           #       )
           #     } else {
           #       c(
           #         "treatment",
           #         paste0("strata_new_", 2:4, "_c"),
           #         paste0("batch_", 2:5, "_c")
           #       )
           #     },
           #     response = x
           #   ),
           #   data = if (x %in% balance_covariates_women) {
           #     subset(rmc, batch != 6)
           #   } else {
           #     rmc
           #   }
           # ) |> tidy()
         )
         
         bind_rows(fits, .id = "id")
       })

recruit_regs <- bind_rows(recruit_regs)

recruit_table <- 
  recruit_regs |>
  select(id, estimate, std.error, df, outcome) |>
  pivot_longer(c(estimate, std.error)) |>
  mutate(
    id = case_when(
      id == 1 ~ "SMA", 
      id == 2 ~ "MIMP",
      id == 3 ~ "RDD"
    ),
    value = ifelse(name == "estimate", as.character(specd(value, 3)), paste0("(", specd(value, 3), ")")),
    df = replace(df, name == "std.error", NA),
    outcome = rep(names(recruit_covariates), each = 6)
  ) |>
  pivot_wider(names_from = id, values_from = c(df, value)) |> 
  mutate(outcome = replace(outcome, name == "std.error", NA)) |>
  select(outcome, df_SMA, value_SMA, df_MIMP, value_MIMP, df_RDD, value_RDD) 


kable(
  x = recruit_table,
  format = "latex",
  col.names = c(" ", "N", "{Mean/SE}", "N", "{Mean/SE}", "N", "{Mean/SE}"),
  align = c("l", "c", "d", "c" , "d", "c", "d"),
  booktabs = TRUE,
  linesep = "",
  caption = "Baseline violence by recruitment source \\label{tab:balance_recruitment}",
  escape = FALSE
) |>
  kable_styling(font_size = 10) |>
  add_header_above(c(
    " " = 1,
    "Social Media" = 2,
    "Ministry" = 2,
    "RDD" = 2
  )) |> 
  footnote(
    general = paste0(
      "This table shows sample characteristics at baseline by source of ",
      "recruitment. Men were recruited via: (1) social media advertising ", 
      "campaign (mostly Facebook), (2) nationally-dispersed promoters ", 
      "employed by the Ministry of Women, or (3) random digit dialing (RDD) using ",
      "numbers from a verified, nationally representative, phone database. ",
      "Columns show mean and robust standard errors (HC2) by source."
      ),
    symbol = "",
    symbol_manual = "",
    general_title = "",
    threeparttable = TRUE,
    escape = FALSE
  ) |>
  save_kable("HEP-manuscript/tables/recruit.tex")
