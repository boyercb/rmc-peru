library(DeclareDesign)
library(tidyverse)
library(ICC)

# declare design function -------------------------------------------------

# design function for the Real Man Challenge evaluation in Peru
rmc_evaluation <- function(n_hosts = 150,
                           n_per_host = 10,
                           # min_per_host = 5,
                           # max_per_host = 10,
                           icc = 0.10,
                           rho = 0.6,
                           mde = 0.10) {
  
  population <- declare_population(
    hosts = add_level(
      N = n_hosts,
      n_couples = n_per_host #round(runif(N, min_per_host, max_per_host))
    ),
    couples = add_level(
      N = n_couples
    ),
    individuals = add_level(
      N = 1,
      u_el = draw_normal_icc(
        mean = 0,
        clusters = hosts,
        ICC = icc,
        sd = 1
      ),
      u_bl = correlate(
        draw_handler = rnorm,
        mean = 0,
        given = u_el,
        rho = rho
      ) 
    )
  )
  
  potential_outcomes <- declare_potential_outcomes(
    Y ~ mde * Z + u_el 
  )
  
  assignment <- declare_assignment(
    handler = function(data) {
      data$Z <- with(
        data,
        cluster_ra(clusters = hosts)
      )
      return(data)
    }
  )
  
  estimand <- declare_estimand(ate = mean(Y_Z_1 - Y_Z_0))
  
  reveal_Y <- declare_reveal(Y,Z)
  
  ols_no_bl <- declare_estimator(
    Y ~ Z,
    model = lm_robust,
    clusters = hosts,
    term = Z,
    estimand = c(estimand),
    label = "ols"
  )
  
  ols_w_bl <- declare_estimator(
    Y ~ Z + u_bl,
    model = lm_robust,
    clusters = hosts,
    term = Z,
    estimand = c(estimand),
    label = "ols with baseline"
  )
  
  design <- 
    population + 
    potential_outcomes + 
    assignment +
    estimand +
    reveal_Y + 
    ols_no_bl +
    ols_w_bl
  
  return(design)
}


# simulation parameters ---------------------------------------------------

rmc_design_comparisons <- expand_design(
  designer = rmc_evaluation,
  expand = TRUE,
  n_hosts = seq(150, 250, length.out = 6),
  n_per_host = c(8, 10, 12),
  icc = seq(0.05, 0.20, 0.05),
  rho = 0.6,
  mde = c(0.1, 0.2)
)


# run simulations ---------------------------------------------------------

rmc_sims <- diagnose_design(
  rmc_design_comparisons,
  sims = 1000,
  bootstrap_sims = FALSE
)

rmc_sims_plot <- rmc_sims$diagnosands_df


# plot results ------------------------------------------------------------

specd <- function(x, k) trimws(format(round(x, k), nsmall=k))

png("rmc_power_plots.png", width = 13, height = 8, units = "in", res = 500)
ggplot(
  rmc_sims_plot,
  aes(
    x = n_hosts,
    y = power,
    color = estimator_label,
    alpha = factor(mde, labels = c("0.1 SD", "0.2 SD")),
    shape = factor(mde, labels = c("0.1 SD", "0.2 SD"))
  )
) +
  facet_grid(
    factor(n_per_host, labels = paste0("N per host = ", c(8, 10, 12))) ~ 
    factor(icc, labels = paste0("ICC = ", seq(0.05, 0.20, 0.05)))
  ) +
  geom_hline(
    yintercept = .8,
    linetype = 2,
    color = "black",
    size = .2
  ) +
  geom_point() +
  geom_line() + 
  geom_text(
    aes(
      label = specd(power, 2),
      y = case_when(
        estimator_label == "ols" & mde == 0.2 ~ power - 0.05,
        estimator_label == "ols with baseline" & mde == 0.2 ~ power + 0.05,
        estimator_label == "ols" & mde == 0.1 ~ power - 0.05,
        estimator_label == "ols with baseline" & mde == 0.1  & power < 0.85 ~ power + 0.05,
        estimator_label == "ols with baseline" & mde == 0.1  & power >= 0.85 ~ power - 0.05
      )
    ),
    data = filter(rmc_sims_plot, n_hosts %in% c(150, 250)),
    size = 2) +
  scale_shape_manual(name = "MDE", values = c(1, 16)) +
  scale_alpha_manual(name = "MDE", values = c(0.25, 1)) +
  scale_color_manual(
    "ICC",
    values = c("dark orange", "dark green"),
    labels = c("ols", bquote("ols with baseline ("*rho*" = 0.6)"))
    ) +
  scale_y_continuous(name = "Power\n",
                     limits = c(0, 1.08),
                     breaks = c(.8, .5, 0, 1)) +
  scale_x_continuous(name = "\nNumber of hosts", breaks = unique(rmc_sims_plot$n_hosts)) +
  labs(
    title = "Power simulations for Real Man Challenge",
    caption = "Note: Based on 1,000 simulations generated using the DeclareDesign package in R."
  ) +
  theme_bw() + 
  theme(
    strip.background = element_blank(), 
    panel.grid = element_blank(), #element_line(color = '#f5f5f5'),
    legend.position = "bottom",
    text = element_text(family = "Palatino")
    )
dev.off()





















