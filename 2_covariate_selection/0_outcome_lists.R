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
  "any_severe",
  "any_severe_physical",
  "any_severe_sexual",
  "ipv_z",
  "physical_z",
  "sexual_z",
  "control_z",
  "psychological_z",
  "cyber_z"
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
  "Any Severe",
  "Any Phys Severe",
  "Any Sex Severe",
  "IPV Z-score",
  "Physical Z-score",
  "Sexual Z-score",
  "Control Z-score",
  "Psych. Z-score",
  "Cyber Z-score"
)

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
    violence_labels,
    primary_labels,
    secondary_labels,
    #time_to_violence_labels,
    demand_labels
  )


attrition_outcomes <- c(
  "responded_m",
  "responded_w"
)
