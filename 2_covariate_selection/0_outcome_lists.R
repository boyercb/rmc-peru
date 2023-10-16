violence_outcomes <- c(
  "any_ipv",
  "any_physical",
  "any_sexual",
  "any_control",
  "any_psychological",
  "any_cyber",
  "ipv_score",
  "physical_score",
  "sexual_score",
  "control_score",
  "psychological_score",
  "cyber_score",
  "any_sometimes",
  "any_sometimes_physical",
  "any_sometimes_sexual",
  "any_often",
  "any_often_physical",
  "any_often_sexual",
  "ipv_breadth",
  "physical_breadth",
  "sexual_breadth",
  "ipv_z",
  "physical_z",
  "sexual_z",
  "control_z",
  "psychological_z",
  "cyber_z",
  "ipv_severity_moderate",
  "ipv_severity_severe"
)

violence_labels <- c(
  "Any IPV",
  "Any Physical",
  "Any Sexual",
  "Any Control",
  "Any Psych.",
  "Any Cyber",
  "IPV Index",
  "Physical Index",
  "Sexual Index",
  "Control Index",
  "Psych. Index",
  "Cyber Index",
  "\\\\shortstack{Any IPV \\\\\\\\ $\\\\geq$ Sometimes}",
  "\\\\shortstack{Any Phys \\\\\\\\ $\\\\geq$ Sometimes}",
  "\\\\shortstack{Any Sex \\\\\\\\ $\\\\geq$ Sometimes}",
  "\\\\shortstack{Any IPV \\\\\\\\ $\\\\geq$ Often}",
  "\\\\shortstack{Any Phys \\\\\\\\ $\\\\geq$ Often}",
  "\\\\shortstack{Any Sex \\\\\\\\ $\\\\geq$ Often}",
  "IPV Breadth",
  "Physical Breadth",
  "Sexual Breadth",
  "IPV Z-score",
  "Physical Z-score",
  "Sexual Z-score",
  "Control Z-score",
  "Psych. Z-score",
  "Cyber Z-score",
  "Moderate IPV",
  "Severe IPV"
)

violence_labels_no_stack <- 
  gsub("\\\\shortstack{", "", violence_labels, fixed = T)
violence_labels_no_stack <- 
  gsub("\\\\geq", "\\geq", violence_labels_no_stack, fixed = T)
violence_labels_no_stack <- 
  gsub("\\\\\\\\", "", violence_labels_no_stack, fixed = T)
violence_labels_no_stack <- 
  gsub("Sometimes}", "Sometimes", violence_labels_no_stack, fixed = T)
violence_labels_no_stack <- 
  gsub("Often}", "Often", violence_labels_no_stack, fixed = T)

time_to_violence_outcomes <- c(
  "ipv_control_lastime_w",
  "ipv_psych_lastime_w",
  "ipv_phys_lastime_w",
  "ipv_sex_lastime_w"
)

time_to_violence_labels <- c(
  "Last time control",
  "Last time psych.",
  "Last time physical",
  "Last time sex"
)


primary_outcomes <- c(
  "control_index",
  "consent_index",
  "comm_index"
)

primary_labels <- c(
  "Control \\\\& DM Index",
  "Consent Index",
  "Comm. Index"
)

primary_labels_no_stack <- 
  c(
    "Control & DM Index",
    "Consent Index",
    "Comm. Index"
  )

secondary_outcomes <- c(
  "satisfaction_m",
  "satisfaction_w", 
  "satisfaction_m_perc_w",
  "satisfaction_w_perc_m",
  "brokeup",
  "arguments",
  "attitudes_m",
  "attitudes_w",
  "depression_m",
  "alcohol_man_w",
  "ladder1_w"
)

secondary_labels <- c(
  "Satisfaction (M)",
  "Satisfaction (W)",
  "Partner's Satisfaction (M)",
  "Partner's Satisfaction (W)",
  "Broke up (W)",
  "Arguments (M)",
  "VAW attitudes (M)",
  "Gender attitudes (W)",
  "Depression (M)",
  "Man's alcohol use (W)",
  "Decision-making power (W)"
)

demand_outcomes <- c(
  "bias_v1_w",
  "bias_v1_m",
  "marlowe_w",
  "marlowe_m"
)

demand_labels <- c(
  "Donate to charity (W)", 
  "Donate to charity (M)", 
  "Social Desirability (W)", 
  "Social Desirability (M)"
)

outcomes <-
  c(
    violence_outcomes,
    primary_outcomes,
    secondary_outcomes,
    #time_to_violence_outcomes,
    demand_outcomes
  )

outcome_labels <-
  c(
    violence_labels_no_stack,
    primary_labels_no_stack,
    secondary_labels,
    #time_to_violence_labels,
    demand_labels
  )


attrition_outcomes <- c(
  "responded_m",
  "responded_w"
)
