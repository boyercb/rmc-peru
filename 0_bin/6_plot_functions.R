

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
plot_coefs <- function(data, facets = NULL, levels = NULL, subgroups = NULL, xlabel = "\nTreatment effect estimate") {
  
  unique_outcomes <- length(unique(data$outcome))
  
  # order outcome factor name by facet + estimate or just estimate
  if (!is.null(facets)) {
    if (!is.null(subgroups)) {
      data$outcome <- 
        factor(
          data$outcome, 
          levels = data$outcome[order(data[[subgroups]], data[[facets]], data$estimate)][1:unique_outcomes]
        )
    } else {
      data$outcome <- 
        factor(
          data$outcome, 
          levels = data$outcome[order(data[[facets]], data$estimate)]
        )
    }

    data$facet <- factor(data[[facets]], levels = levels)
  } else {
    if (!is.null(subgroups)) {
      data$outcome <-
        with(data, factor(outcome, levels = outcome[order(data[[subgroups]], estimate)][1:unique_outcomes]))
    } else {
      data$outcome <-
        with(data, factor(outcome, levels = outcome[order(estimate)]))
    }
  }
  
  
  # initiate plot
  if (is.null(subgroups)) {
    p <- ggplot(data, aes(x = outcome, y = estimate)) 
  } else {
    p <-
      ggplot(data,
             aes(
               x = outcome,
               y = estimate,
               fill = .data[[subgroups]],
               color = .data[[subgroups]]
             ))
  }
  
  # add facet if specified
  if (!is.null(facets)) {
    p <-
      p + facet_grid(facet ~ .,
                     scales = "free_y",
                     space = "free_y",
                     switch = "y")
  } 
  
  if (!is.null(subgroups)) {
    p <- 
      p + geom_point(size = 2,
                     position = position_dodge2(width = 1)) +
      geom_hline(yintercept = 0,
                 linetype = "dashed",
                 linewidth = .5) +
      geom_linerange(aes(
        ymin = conf.low,
        ymax = conf.high,
        x = outcome
      ),
      alpha = .3,
      linewidth = 1.5,
      position = position_dodge2(width = 1)) +
      scale_color_discrete(name = "") +
      scale_fill_discrete(name = "")
  } else {
    p <- 
      p + geom_point(size = 2) +
      geom_hline(yintercept = 0,
                 linetype = "dashed",
                 linewidth = .5) +
      geom_linerange(aes(
        ymin = conf.low,
        ymax = conf.high,
        x = outcome
      ),
      alpha = .3,
      linewidth = 1.5) 
  }
    
  # finish plot
  p + 
    coord_flip() +
    labs(
      y = xlabel,
      x = NULL
    ) +
    rmc_theme() +
    theme(
      strip.clip = "off",
      legend.position = "top"
    )
}


