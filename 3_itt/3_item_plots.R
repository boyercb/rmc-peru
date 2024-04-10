

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


violence_item_models <- 
  lapply(X = violence_items, 
         FUN = function(outcome, adjusted = TRUE) {
           strata_FE <- c(
             paste0("strata_new_", 2:4, "_c"),
             paste0("batch_", 2:5, "_c")
           )
           
           y_covs <- y_selected$covariate[y_selected$outcome == outcome]
           z_covs <- z_selected$covariate
           covs <- unique(c(y_covs, z_covs, strata_FE))
           
           lm_robust(
             reformulate(
               termlabels = if (adjusted) {
                 c("treatment", covs, paste0("treatment:", covs))
               } else {
                 c("treatment", strata_FE, paste0("treatment:", strata_FE))
               },
               response = paste0("I(", outcome, "> 1)"),
             ),
             data = filter(rmc, id_status_w == 1)
           )     
         })

violence_item_plot_data <- 
  lapply(X = violence_item_models, FUN = tidy) %>% 
  do.call(what = rbind, args = .) %>% 
  filter(term == "treatment")

violence_item_plot_data$outcome <- violence_item_descriptions
violence_item_plot_data$index <- c(
  rep("Any\nControl", 4),
  rep("Any\nPsych.", 2),
  rep("Any\nPhysical", 4),
  rep("Any\nSexual", 2),
  rep("Any\nCyber", 3)
)

violence_item_plot <- plot_coefs(
  data = violence_item_plot_data, 
  facets = "index", 
  levels = c(
    "Any\nControl",
    "Any\nPsych.",
    "Any\nPhysical",
    "Any\nSexual",
    "Any\nCyber"
  )) 

ggsave(
  filename = "7_figures/violence_item_plot.pdf",
  plot = violence_item_plot,
  device = "pdf",
  width = 6,
  height = 6
)

# make a table as well


# other primary outcomes --------------------------------------------------

control_item_descriptions <- c(
  "what to cook (w)",
  "visit friends/family (w)",
  "spending her earnings (w)",
  "what to wear (w)",
  "spending his earnings (w)",
  "healthcare (w)",
  "major purchases (w)"
)

control_item_models <- 
  lapply(X = control_items, 
         FUN = function(outcome, adjusted = TRUE) {
           strata_FE <- c(
             paste0("strata_new_", 2:4, "_c"),
             paste0("batch_", 2:5, "_c")
           )
           
           y_covs <- y_selected$covariate[y_selected$outcome == outcome]
           # r_covs <- r_selected$covariate
           # if (outcome == "arguments") {
           #   r_covs <- NULL
           # }
           z_covs <- z_selected$covariate
           covs <- unique(c(y_covs, z_covs, strata_FE))
           lm_robust(
             reformulate(
               termlabels = if (adjusted) {
                 c("treatment", covs, paste0("treatment:", covs))
               } else {
                 c("treatment", strata_FE, paste0("treatment:", strata_FE))
               },
               response = outcome,
             ),
             #fixed_effects = ~factor(strata),
             data = filter(rmc, id_status_w == 1)
           )     
         })

control_item_plot_data <- 
  lapply(X = control_item_models, FUN = tidy) %>% 
  do.call(what = rbind, args = .) %>% 
  filter(term == "treatment")

control_item_plot_data$outcome <- control_item_descriptions

consent_item_descriptions <- c(
  "controls when to have sex (w)",
  "confident saying no (w)"
)

consent_item_models <- 
  lapply(X = consent_items, 
         FUN = function(outcome, adjusted = TRUE) {
           strata_FE <- c(
             paste0("strata_new_", 2:4, "_c"),
             paste0("batch_", 2:5, "_c")
           )
           
           y_covs <- y_selected$covariate[y_selected$outcome == outcome]
           # r_covs <- r_selected$covariate
           # if (outcome == "arguments") {
           #   r_covs <- NULL
           # }
           z_covs <- z_selected$covariate
           covs <- unique(c(y_covs, z_covs, strata_FE))
           lm_robust(
             reformulate(
               termlabels = if (adjusted) {
                 c("treatment", covs, paste0("treatment:", covs))
               } else {
                 c("treatment", strata_FE, paste0("treatment:", strata_FE))
               },
               response = outcome,
             ),
             #fixed_effects = ~factor(strata),
             data = filter(rmc, id_status_w == 1)
           )      
         })

consent_item_plot_data <- 
  lapply(X = consent_item_models, FUN = tidy) %>% 
  do.call(what = rbind, args = .) %>% 
  filter(term == "treatment")

consent_item_plot_data$outcome <- consent_item_descriptions


comm_item_descriptions <- c(
  "discuss woman's day (w)",
  "discuss man's day (w)",
  "discuss woman's concerns (w)",
  "discuss man's concerns (w)",
  "never interrupts (w)",
  "listens (w)",
  "encourages (w)",
  "thanks (w)",
  "never minimizes (w)",
  "expresses needs (w)",
  "man understands needs (w)",
  "how argue: listens (w)",
  "how argue: doesn't insult (w)",
  "how argue: doesn't threaten (w)",
  "how argue: cools off (w)",
  "discuss man's day (m)",
  "discuss woman's day (m)",
  "discuss man's concerns (m)",
  "discuss woman's concerns (m)"
  # "argue about responsibilities (m)",
  # "argue about financial supp. (m)",
  # "argue about money (m)",
  # "argue about alcohol (m)",
  # "argue about infedility (m)",
  # "argue about sex (m)",
  # "argue about children (m)"
)

comm_item_models <- 
  lapply(X = comm_items, 
         FUN = function(outcome, adjusted = TRUE) {
           strata_FE <- c(
             paste0("strata_new_", 2:4, "_c"),
             paste0("batch_", 2:5, "_c")
           )
           
           y_covs <- y_selected$covariate[y_selected$outcome == outcome]
           # r_covs <- r_selected$covariate
           # if (outcome == "arguments") {
           #   r_covs <- NULL
           # }
           z_covs <- z_selected$covariate
           covs <- unique(c(y_covs, z_covs, strata_FE))
           lm_robust(
             reformulate(
               termlabels = if (adjusted) {
                 c("treatment", covs, paste0("treatment:", covs))
               } else {
                 c("treatment", strata_FE, paste0("treatment:", strata_FE))
               },
               response = outcome,
             ),
             #fixed_effects = ~factor(strata),
             data = filter(rmc, id_status_w == 1)
           )      
         })

comm_item_plot_data <- 
  lapply(X = comm_item_models, FUN = tidy) %>% 
  do.call(what = rbind, args = .) %>% 
  filter(term == "treatment")

comm_item_plot_data$outcome <- comm_item_descriptions

primary_item_plot_data <- bind_rows(
  control_item_plot_data,
  consent_item_plot_data,
  comm_item_plot_data
)


primary_item_plot_data$index <- c(
  rep("Control & DM Index", length(control_items)),
  rep("Consent\nIndex", length(consent_items)),
  rep("Comm. Index", 19)
)

primary_item_plot <- plot_coefs(
  data = primary_item_plot_data, 
  facets = "index", 
  levels = c(
    "Control & DM Index",
    "Consent\nIndex",
    "Comm. Index"
  )) 

ggsave(
  filename = "7_figures/primary_item_plot.pdf",
  plot = primary_item_plot,
  device = "pdf",
  width = 6,
  height = 8.5
)


# pdf("09_endline_manuscript/figures/any_violence_coefficients.pdf", width = 5, height = 5)
# plot_coefs(any_violence_plot_data) %>% print()
# dev.off()



# justification  ----------------------------------------------------------

tolerance_item_descriptions <- c(
  "Si una mujer le falta el respeto a su esposo o pareja, merece algún tipo de castigo.",
  "Un hombre que está celoso con su esposa o pareja es porque esto demuestra que la ama.",
  "Una mujer que se viste provocativamente y con ropa reveladora busca ser acosada sexualmente.",
  "Una mujer infiel a su esposo o pareja debe tener algún tipo de castigo.",
  "Una mujer siempre debe estar dispuesta a tener relaciones sexuales cuando su esposo o pareja lo desee."
)


tolerance_item_models <- 
  lapply(X = attitude_items_m, 
         FUN = function(outcome, adjusted = F) {
           strata_FE <- c(
             # paste0("strata_new_", 2:4, "_c"),
             paste0("batch_", 2:5, "_c")
           )
           
           y_covs <- y_selected$covariate[y_selected$outcome == "attitudes_m"]
           # r_covs <- r_selected$covariate
           # if (outcome == "arguments") {
           #   r_covs <- NULL
           # }
           z_covs <- z_selected$covariate
           covs <- unique(c(y_covs, z_covs, strata_FE))
           
           lm_robust(
             reformulate(
               termlabels = if (adjusted) {
                 c("treatment", covs, paste0("treatment:", covs))
               } else {
                 c("treatment", strata_FE, paste0("treatment:", strata_FE))
               },
               response = paste0("I(", outcome, "> 1)"),
             ),
             #fixed_effects = ~factor(strata),
             data = filter(rmc, id_status_w == 1 & strata_new %in% c(2,3)),
             alpha = 0.10
           )     
         })

tolerance_item_plot_data <- 
  lapply(X = tolerance_item_models, FUN = tidy) %>% 
  do.call(what = rbind, args = .) %>% 
  filter(term == "treatment")

tolerance_item_plot_data$outcome <- str_wrap(tolerance_item_descriptions, 20)

tolerance_item_plot <- plot_coefs(
  data = tolerance_item_plot_data
) 

ggsave(
  filename = "7_figures/tolerance_item_plot.pdf",
  plot = tolerance_item_plot,
  device = "pdf",
  width = 6,
  height = 6
)

# make a table as well
