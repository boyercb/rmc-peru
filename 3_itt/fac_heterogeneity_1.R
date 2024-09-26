group_outcomes <- c(
  violence_outcomes[2:3]
)

group_level_effects <- pmap_df(
  list(
    g = rep(1:27, length(group_outcomes)),
    b = rep(c(rep(1:5, each = 5), rep(6, each = 2)), length(group_outcomes)),
    o = rep(group_outcomes, each = 27)
  ), 
  function(g, b, o) {
    fit <- lm_lin(
      formula = reformulate("treatment", o),
      covariates = reformulate(paste0("strata_new_", 3)),
      data = subset(rmc, id_status_w == 1 & batch == b & (group == g | group == 0)),
      alpha = 0.10 
    )
    
    res <- tidy(fit)
    res$batch <- b
    res$group <- (g - 1) %% 5 + 1
    res$fl <- 0
    
    return(res)
  }
)

fac_outcomes <- c(
  violence_outcomes[2:3],
  coded_outcomes 
)

fac_level_effects <- pmap_df(
  list(
    b = rep(1:6, length(fac_outcomes)),
    o = rep(fac_outcomes, each = 6),
    cluster = c(rep(FALSE, 6 * 2), rep(TRUE, 6 * (length(fac_outcomes) - 2)))
  ),
  function(b, o, cluster) {
    if (cluster) {
      fit <- lm_robust(
        formula = reformulate("1", o),
        # covariates = reformulate(paste0("strata_new_", 3)),
        data = subset(rmc, id_status_w == 1 & batch == b & treatment == 1),
        clusters = group,
        alpha = 0.10 
      )
    } else {
      fit <- lm_lin(
        formula = reformulate("treatment", o),
        covariates = reformulate(paste0("strata_new_", 3)),
        data = subset(rmc, id_status_w == 1 & batch == b),
        alpha = 0.10 
      )
    }
    
    res <- tidy(fit)
    res$batch <- b
    res$group <- 0
    res$fl <- 1
    
    return(res)
  })

fac_group_effects <- bind_rows(group_level_effects, fac_level_effects)

plt <- filter(
  fac_group_effects,
  term == "treatment" & outcome %in% c("any_physical", "any_sexual")
)

plt$outcome_name <- plt$outcome
plt$outcome <- plt$group
plt$outcome_name <- factor(plt$outcome_name, labels = violence_labels[2:3])
plt$batch <- factor(plt$batch, labels = paste0("FL: ", 1:6))

p <- 
  ggplot(plt,
  aes(
    x = factor(outcome, labels = c("", 1:5)),
    y = estimate,
    color = factor(fl),
    fill = factor(fl),
    shape = factor(fl)
  )) +
  facet_grid(batch ~ outcome_name, 
             drop = TRUE, 
             scales = "free_y", space = "free_y") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             linewidth = .5) +
  geom_linerange(aes(
    ymin = conf.low,
    ymax = conf.high,
  ),
  alpha = .65,
  linewidth = 1.25) + 
  geom_point(size = 2.5) +
  coord_flip() +
  rmc_theme() +
  labs(
    y = "Effect of HEP",
    x = "Group"
  ) +
  scale_shape_manual(values = c(21, 23)) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  theme(
    strip.clip = "off",
    legend.position = "nonw",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggsave(
  filename = "HEP-manuscript/figures/facilitator_heterogeneity.pdf", 
  plot = p, 
  device = "pdf",
  width = 6.5,
  height = 6.5
)




plt <- filter(
  fac_group_effects,
  term == "(Intercept)" & (!outcome %in% c("any_physical", "any_sexual"))
)

plt$outcome_name <- plt$outcome
plt$outcome <- plt$group
plt$outcome_name <-
  factor(str_remove(str_remove(plt$outcome_name, "_rev_g"), "_partner"),
         levels = str_remove(str_remove(coded_outcomes, "_rev_g"), "_partner"),
         labels = str_remove(str_remove(coded_outcomes, "_rev_g"), "_partner"))
plt$batch <- factor(plt$batch, labels = paste0("FL: ", 1:6))

p <- 
  ggplot(filter(plt, batch != "FL: 6"),
         aes(
           x = batch,
           y = estimate,
           color = factor(fl),
           fill = factor(fl),
           shape = factor(fl)
         )) +
  facet_wrap( ~ outcome_name,
             drop = TRUE, 
             scales = "free_x") +
  # geom_hline(yintercept = 0,
  #            linetype = "dashed",
  #            linewidth = .5) +
  geom_linerange(aes(
    ymin = conf.low,
    ymax = conf.high,
  ),
  alpha = .65,
  linewidth = 1.25) + 
  geom_point(size = 2.5) +
  coord_flip() +
  rmc_theme() +
  labs(
    y = NULL, #"Mean per Facilitator",
    x = "Group"
  ) +
  scale_shape_manual(values = c(21, 23)) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  theme(
    strip.clip = "off",
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggsave(
  filename = "HEP-manuscript/figures/coded_messages_by_fac.pdf", 
  plot = p, 
  device = "pdf",
  width = 6.5,
  height = 6.5
)
# 
# f1 <- lm_lin(
#   formula = any_sexual ~ treatment,
#   covariates = ~ strata_new_2_c + strata_new_3_c + strata_new_4_c + batch_2_c + batch_4_c + batch_5_c,
#   data = subset(rmc, id_status_w == 1 & batch != 3)
# )
# 
# f2 <- lm_lin(
#   formula = any_sexual ~ treatment,
#   covariates = ~ strata_new_2_c + strata_new_3_c + strata_new_4_c,
#   data = subset(rmc, id_status_w == 1)
# )
# 
# strata_FE <- c(
#   paste0("strata_new_", 2:4, "_c"),
#   paste0("batch_", 2:5, "_c")
# )
# 
# lm_robust(
#   reformulate(
#     termlabels = c("treatment", strata_FE, paste0("treatment:", strata_FE)),
#     response = "any_sexual"
#   ),
#   data = subset(rmc, id_status_w == 1)
# )
# 
# waldtest(f1, f2)
# 
# 
