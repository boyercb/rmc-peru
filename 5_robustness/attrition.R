
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
