balance_covariates_men <- c(
  "Man's age" = "age_m_bl",
  "Man has some post-secondary education" =  "I(education_w_bl >= 5)",
  "Years together (reported by man)" = "years_relationship_m_bl",
  "Man is employed" = "he_works_m_bl",
  "Man uses WhatsApp daily" = "I(whatsapp_freq_m_bl == 1)",
  "Man was recruited via social media" =  "I(sample_pi_m_bl == 1)",
  "Conflict index (reported by man)" = "arguments_m_bl",
  "Justification of violence (reported by man)" = "tolerance_vaw_index_bl",
  "Man's father was violent" = "I(violence_parents_m_bl > 1)"
)

balance_covariates_women <- c(
  "Woman's age" = "age_w_bl",
  # "years_cohabiting_w_bl",
  "Woman has some post-secondary education" = "I(education_w_bl >= 5)",
  "Household size" = "hh_members_w_bl",
  "Woman is employed" = "she_works_w_bl",
  "Man drinks alcohol (reported by woman)" = "I(alcohol_man_w_bl > 1)",
  "Decision-making power (reported by woman)" = "ladder1_w_bl",
  "Control index (reported by woman)" = "control_index_bl",
  "Communication index (reported by woman)" = "comm_w_bl",
  "Perceived control over sex (reported by woman)" = "consent_index_bl",
  "Men's ability to self-regulate (reported by woman)" = "emo_reg_w_bl",
  "Justification of violence (reported by woman)" = "attitudes_w_bl",
  "Any psychological violence (reported by woman)" = "any_psychological_bl",
  "Any physical violence (reported by woman)" ="any_physical_bl",
  "Any sexual violence (reported by woman)" = "any_sexual_bl"
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
    balance_covariates_men[8],
    balance_covariates_women[12:14],
    balance_covariates_men[9])

responder_regs <-
  lapply(balance_covariates,
         function(x) {
           fits <- list(
             lm_robust(
               formula = reformulate(termlabels = "1",
                                     response = x),
               data = if (x %in% balance_covariates_women) {
                 subset(rmc, id_status_w != 1 & batch != 6)
               } else {
                 subset(rmc, id_status_m != 1)
               }
             ) |> tidy(),
             lm_robust(
               formula = reformulate(termlabels = "1",
                                     response = x),
               data = if (x %in% balance_covariates_women) {
                 subset(rmc, id_status_w == 1 & batch != 6)
               } else {
                 subset(rmc, id_status_m == 1)
               }
             ) |> tidy(),
             lm_robust(
               formula = reformulate(
                 termlabels = if (x %in% balance_covariates_women) {
                   if (x %in% c("any_physical_bl", "any_sexual_bl")) {
                     c(
                       "I(id_status_w == 1)",
                       paste0("batch_", 2:5, "_c")
                     )
                   } else {
                     c(
                       "I(id_status_w == 1)",
                       paste0("strata_new_", 2:3, "_c"),
                       paste0("batch_", 2:5, "_c")
                     )
                   }
                 } else {
                   c(
                     "I(id_status_m == 1)",
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

responder_regs <- bind_rows(responder_regs)

responder_table <- 
  responder_regs |>
  filter(!(term == "(Intercept)" & id == 3)) |>
  filter(!term %in% c(paste0("strata_new_", 2:4, "_c"),
                      paste0("batch_", 2:5, "_c"))) |>
  select(id, estimate, std.error, df, outcome, p.value) |>
  pivot_longer(c(estimate, std.error)) |>
  mutate(
    id = case_when(
      id == 1 ~ "nonresponder", 
      id == 2 ~ "responder",
      id == 3 ~ "diff"
    ),
    value = ifelse(
      name == "estimate" & id == "diff", add_stars(value, p.value, 2), 
      ifelse(name == "estimate" & id %in% c("responder", "nonresponder"), 
             as.character(specd(value, 2)), 
             paste0("(", specd(value, 3), ")"))),
    df = replace(df, name == "std.error", NA),
    outcome = rep(names(balance_covariates), each = 6)
  ) |>
  select(-p.value) |>
  pivot_wider(names_from = id, values_from = c(df, value)) |> 
  mutate(outcome = replace(outcome, name == "std.error", NA)) |>
  select(outcome, df_responder, value_responder, df_nonresponder, value_nonresponder, value_diff) 

# run the joint test
fit <- lm_robust(
  formula = reformulate(
    termlabels = balance_covariates_women,
    response = "I(id_status_w == 1)"
  ),
  data = rmc
)

fstat <- fit$fstatistic
fpval <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

add_row <- tibble(
  outcome = c(paste0("Joint F-statistic (df1=", round(fstat[2]), ", df2=", round(fstat[3]), ")"),  "P-value"),
  value_diff = c(specd(fstat[1], 3), specd(fpval, 3))
)

responder_table <- bind_rows(responder_table, add_row)

options(knitr.kable.NA = '')


kable(
  x = responder_table,
  format = "latex",
  col.names = c(" ", "N", "{Mean/SE}", "N", "{Mean/SE}", "{Difference}"),
  align = c("l", "c", "d", "c" , "d", "d"),
  booktabs = TRUE,
  linesep = "",
  caption = "Baseline characteristics of responders versus non-responders at follow up \\label{tab:responder}",
  escape = FALSE
) |>
  kable_styling(font_size = 10) |>
  row_spec(
    row = nrow(responder_table) - 2,
    hline_after = TRUE
  ) |>
  add_header_above(c(
    " " = 1,
    "Responder" = 2,
    "Non-responder" = 2,
    " " = 1
  )) |> 
  save_kable("HEP-manuscript/tables/responder.tex")


# 2 -----------------------------------------------------------------------


strata_FE <- paste0("strata_simple_", 1:4)
group <- c(
  "overall", 
  "within strata"
)

rmc <-
  rmc |>
  mutate(
    strata_simple_1 = as.numeric(strata_new %in% c(1, 4)),
    strata_simple_2 = as.numeric(strata_new %in% c(2, 5)),
    strata_simple_3 = as.numeric(strata_new %in% c(3, 6)),
    strata_simple_4 = as.numeric(strata_new == 7)
  )

attrition_tests <-  
  map2(
    .x = rep(attrition_outcomes, 2),
    .y = rep(group, each = 2),
    function(x, y) {
      # strata_FE <- paste0("strata_new_", 1:7)
      
      lm_robust(
        formula = reformulate(
          termlabels = if (y == "within strata") {
            c(strata_FE, paste0("treatment:", strata_FE))
          } else {
            c("treatment")
          },
          intercept = if (y == "within strata") {
            FALSE
          } else {
            TRUE
          },
          response = x
        ),
        data = rmc
      )
    }
  )

names(attrition_tests) <- paste0(rep(attrition_outcomes, 2), ": ", rep(group, each = 2))

attrition_table <- bind_rows(lapply(attrition_tests, tidy), .id = "id")


attrition_table <- 
  attrition_table |>
  mutate(
    gender = if_else(str_detect(outcome, "_w$"), "Woman", "Man"),
    analysis = if_else(str_detect(id, "overall"), "Overall", "Within strata")
  )

attrition_table_panel_a <- 
  attrition_table |>
  filter(analysis == "Overall") |>
  select(gender, term, estimate, p.value) |>
  mutate(term = replace(term, term == "(Intercept)", "control")) |>
  pivot_wider(names_from = term, values_from = c(estimate, p.value)) |>
  mutate(
    estimate_treatment = estimate_treatment + estimate_control,
    diff = estimate_treatment - estimate_control,
    p.value = p.value_treatment
  ) |>
  select(-p.value_control, -p.value_treatment) |>
  pivot_wider(
    names_from = gender,
    values_from = c(estimate_control, estimate_treatment, diff, p.value)
  ) |>
  mutate(term = "", n = nrow(rmc)) |>
  select(term,
         n,
         estimate_treatment_Woman,
         estimate_control_Woman,
         diff_Woman,
         p.value_Woman,
         estimate_treatment_Man,
         estimate_control_Man,
         diff_Man,
         p.value_Man
  ) 

attrition_table_panel_b <- 
  attrition_table |>
  filter(analysis == "Within strata") |>
  select(gender, term, estimate, p.value) |>
  mutate(
    treatment = if_else(str_detect(term, ":treatment"), "treatment", "control"),
    term = str_replace(term, ":treatment", "")
  ) |>
  pivot_wider(names_from = treatment, values_from = c(estimate, p.value)) |>
  mutate(
    estimate_treatment = estimate_treatment + estimate_control,
    diff = estimate_treatment - estimate_control,
    p.value = p.value_treatment
  ) |>
  select(-p.value_control, -p.value_treatment) |>
  pivot_wider(
    names_from = gender,
    values_from = c(estimate_control, estimate_treatment, diff, p.value)
  ) |>
  mutate(
    term = c(
      "No violence",
      "Sexual only",
      "Physical or sexual",
      # "No baseline IPV (batch 1)",
      # "Sexual IPV only (batch 1)",
      # "Physical and sexual IPV (batch 1)",
      "No baseline"
    ),
    n = c(
      sum(rmc$strata_simple_1),
      sum(rmc$strata_simple_2),
      sum(rmc$strata_simple_3),
      sum(rmc$strata_simple_4)
    )
  ) |>
  select(term, 
         n,
         estimate_treatment_Woman,
         estimate_control_Woman,
         diff_Woman,
         p.value_Woman,
         estimate_treatment_Man,
         estimate_control_Man,
         diff_Man,
         p.value_Man
  )


joint_test_p_values <-
  list(
    waldtest(
      lm(
        formula = reformulate(
          termlabels = c("treatment", strata_FE[-1], paste0("treatment:", strata_FE[-1])),
          response = attrition_outcomes[2]
        ),
        data = rmc
      ),
      lm(
        formula = reformulate(
          termlabels = c("treatment", strata_FE[-1]),
          response = attrition_outcomes[2]
        ),
        data = rmc
      ),
      test = "Chisq",
      vcov = function(x) vcovHC(x, type = "HC2")
    )$`Pr(>Chisq)`[2],
    waldtest(
      lm(
        formula = reformulate(
          termlabels = c("treatment", strata_FE[-1], paste0("treatment:", strata_FE[-1])),
          response = attrition_outcomes[1]
        ),
        data = rmc
      ),
      lm(
        formula = reformulate(
          termlabels = c("treatment", strata_FE[-1]),
          response = attrition_outcomes[1]
        ),
        data = rmc
      ),
      test = "Chisq",
      vcov = function(x) vcovHC(x, type = "HC2")
    )$`Pr(>Chisq)`[2]
  )
  
attrition_table_panel_b <- add_row(attrition_table_panel_b, 
                                   term = "Joint $F$-test",
                                   n = NA,
                                   estimate_treatment_Woman = NA,
                                   estimate_control_Woman = NA,
                                   diff_Woman = NA,
                                   p.value_Woman = joint_test_p_values[[1]],
                                   estimate_treatment_Man = NA,
                                   estimate_control_Man = NA,
                                   diff_Man = NA,
                                   p.value_Man = joint_test_p_values[[2]])
  
options(knitr.kable.NA = '')

kable(
  x = bind_rows(attrition_table_panel_a, attrition_table_panel_b),
  format = "latex",
  digits = 3,
  col.names = c(
    "",
    "N",
    "T",
    "C",
    "T - C",
    "P-value",
    "T",
    "C",
    "T - C",
    "P-value"
  ),
  align = "lccccccccc",
  booktabs = TRUE,
  linesep = '',
  escape = FALSE
) |>
  kable_styling(latex_options = "hold_position") |>
  add_header_above(c(" " = 2, "Women's Response Rate" = 4, "Men's Response Rate" = 4)) |>
  group_rows("Panel A. Overall", 1, 1) |>
  group_rows("Panel B. Within baseline violence strata", 2, nrow(attrition_table_panel_b) + 1) |>
  row_spec(nrow(attrition_table_panel_b) + 1, italic = TRUE) |>
  save_kable(
    file = "6_tables/attrition.tex"
  )
