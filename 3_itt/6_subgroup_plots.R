# violence  ---------------------------------------------------------------

violence_item_descriptions <- c(
  "strong arguments",
  "jealousy",
  "control cell phone",
  "control movements",
  "humiliate",
  "threatened",
  "pushed, shook, threw",
  "slapped or twisted",
  "hit with fist",
  "kicked or dragged",
  "forced sex",
  "forced acts",
  "reviewed social media",
  "reviewed messages",
  "shared intimate photos"
)

subgroup_violence_items_models <- map(
  violence_items,
  function(x) {
    fit_ipv <- 
      lm_robust(
        formula = reformulate(
          termlabels = c(
            "treatment"
          ),
          response = paste0("I(", x, "> 1)")
        ),
        data = subset(rmc, id_status_w == 1 & strata_new %in% c(2,3)),
        alpha = 0.10
      )
    
    fit_no_ipv <- 
      lm_robust(
        formula = reformulate(
          termlabels = c(
            "treatment"
          ),
          response = paste0("I(", x, "> 1)")
        ),
        data = subset(rmc, id_status_w == 1 & strata_new %in% c(1)),
        alpha = 0.10
      )
    
    fit_diff <- 
      lm_robust(
        formula = reformulate(
          termlabels = c(
            "treatment",
            "I(strata_new %in% c(2, 3))",
            paste0("treatment:", "I(strata_new %in% c(2, 3))")
          ),
          response = paste0("I(", x, "> 1)")
        ),
        data = subset(rmc, id_status_w == 1),
        alpha = 0.10
      )
    
    fits <- list(
      "Baseline IPV" = fit_ipv,
      "No baseline IPV" = fit_no_ipv,
      "Diff" = fit_diff
    ) 
    
    bind_rows(lapply(fits, tidy), .id = "id")
  })

subgroup_violence_item_plot_data <- 
  bind_rows(subgroup_violence_items_models) |>
  filter(
    (term == "treatment" & id %in% c("Baseline IPV", "No baseline IPV")) | 
      (term == "treatment:I(strata_new %in% c(2, 3))TRUE" & id == "Diff")
    ) |>
  select(id, estimate, std.error, p.value, conf.low, conf.high, outcome) |>
  mutate(
    outcome = rep(violence_item_descriptions, each = 3),
    index = c(
      rep("Any\nControl", 12),
      rep("Any\nPsych.", 6),
      rep("Any\nPhysical", 12),
      rep("Any\nSexual", 6),
      rep("Any\nCyber", 9)
    )
  )

subgroup_violence_item_plot <- plot_coefs(
  data = filter(subgroup_violence_item_plot_data, id != "Diff"),
  facets = "index", 
  subgroups = "id",
  levels = c(
    "Any\nControl",
    "Any\nPsych.",
    "Any\nPhysical",
    "Any\nSexual",
    "Any\nCyber"
  )) 

ggsave(
  filename = "7_figures/subgroup_violence_item_plot.pdf",
  plot = subgroup_violence_item_plot,
  device = "pdf",
  width = 6,
  height = 6
)


# re-split ----------------------------------------------------------------

subgroup2_violence_items_models <- map(
  violence_items,
  function(x) {
    fit_no_proxy <- 
      lm_robust(
        formula = reformulate(
          termlabels = c(
            "treatment"
          ),
          response = paste0("I(", x, "> 1)")
        ),
        data = subset(rmc, id_status_w == 1 & strata_new %in% c(1) & communication9_w_bl_0 == 1),
        alpha = 0.10
      )
    
    fit_proxy <- 
      lm_robust(
        formula = reformulate(
          termlabels = c(
            "treatment"
          ),
          response = paste0("I(", x, "> 1)")
        ),
        data = subset(rmc, id_status_w == 1 & strata_new %in% c(1) & communication9_w_bl_0 == 0),
        alpha = 0.10
      )
    
    fit_diff <- 
      lm_robust(
        formula = reformulate(
          termlabels = c(
            "treatment",
            "I(communication9_w_bl_0 == 0)",
            paste0("treatment:", "I(communication9_w_bl_0 == 0)")
          ),
          response = paste0("I(", x, "> 1)")
        ),
        data = subset(rmc, id_status_w == 1 & strata_new %in% c(1)),
        alpha = 0.10
      )
    
    fits <- list(
      "Never minimizes" = fit_no_proxy,
      "Minimizes" = fit_proxy,
      "Diff" = fit_diff
    ) 
    
    bind_rows(lapply(fits, tidy), .id = "id")
  })

subgroup_violence_item_plot_data <- 
  bind_rows(subgroup_violence_items_models) |>
  filter(
    (term == "treatment" & id %in% c("Baseline IPV", "No baseline IPV")) | 
      (term == "treatment:I(strata_new %in% c(2, 3))TRUE" & id == "Diff")
  ) |>
  select(id, estimate, std.error, p.value, conf.low, conf.high, outcome) |>
  mutate(
    outcome = rep(violence_item_descriptions, each = 3),
    index = c(
      rep("Any\nControl", 12),
      rep("Any\nPsych.", 6),
      rep("Any\nPhysical", 12),
      rep("Any\nSexual", 6),
      rep("Any\nCyber", 9)
    )
  )

subgroup_violence_item_plot <- plot_coefs(
  data = filter(subgroup_violence_item_plot_data, id != "Diff"),
  facets = "index", 
  subgroups = "id",
  levels = c(
    "Any\nControl",
    "Any\nPsych.",
    "Any\nPhysical",
    "Any\nSexual",
    "Any\nCyber"
  )) 

ggsave(
  filename = "7_figures/subgroup_violence_item_plot.pdf",
  plot = subgroup_violence_item_plot,
  device = "pdf",
  width = 6,
  height = 6
)
