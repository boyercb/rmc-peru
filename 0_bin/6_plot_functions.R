

# create custom theme function
rmc_theme <- 	function() {
  theme_bw(base_size = 12) +
    theme(
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_line(color = '#eeeeee'),
      strip.background = element_blank(),
      legend.position = "bottom",
      text = element_text(family = "Palatino"),
      strip.placement = "outside"
    )
}


# function to create nice plot
plot_coefs <- function(data, facets = NULL, levels = NULL) {
  
  # order outcome factor name by facet + estimate or just estimate
  if (!is.null(facets)) {
    data$outcome <- 
      factor(
        data$outcome, 
        levels = data$outcome[order(data[[facets]], data$estimate)]
      )

    data$facet <- factor(data[[facets]], levels = levels)
  } else {
    data$outcome <-
      with(data, factor(outcome, levels = outcome[order(estimate)]))
    
  }
  
  
  # initiate plot
  p <- ggplot(data, aes(y = outcome, x = estimate)) 
  
  # add facet if specified
  if (!is.null(facets)) {
    p <-
      p + facet_grid(facet ~ .,
                     scales = "free_y",
                     space = "free_y",
                     switch = "y")
  } 
    
  # finish plot
  p + 
    geom_point(size = 2) +
    geom_vline(xintercept = 0,
               linetype = "dashed",
               linewidth = .5) +
    geom_segment(aes(
      x = conf.low,
      xend = conf.high,
      y = outcome,
      yend = outcome
    ),
    alpha = .3,
    linewidth = 1.5) +
    labs(
      x = "\nTreatment effect estimate",
      y = NULL
    ) +
    rmc_theme() +
    theme(strip.clip = "off")
}


