


p <- pivot_longer(select(recoded_messages_to_merge, ends_with("_g"), participant_id),
             -participant_id) |>
  ggplot(aes(x = value)) +
  facet_wrap(~str_remove(name, "_rev_g"),  scales = "free") +
  geom_histogram(bins = 30) +
  theme_bw() +
  labs(
    x = NULL,
    y = NULL
  )

ggsave(
  filename = "HEP-manuscript/figures/coded_messages_histogram.pdf",
  plot = p,
  device = "pdf",
  width = 6.5, 
  height = 5
)

p <- pivot_longer(select(recoded_messages_to_merge, ends_with("_g"), participant_id),
                  -participant_id) |>
  ggplot(aes(x = value)) +
  facet_wrap(~str_remove(str_remove(name, "_rev_g"), "_partner"),  scales = "free") +
  geom_histogram(bins = 30) +
  theme_bw() +
  labs(
    x = NULL,
    y = NULL
  )

ggsave(
  filename = "HEP-manuscript/figures/coded_messages_histogram.pdf",
  plot = p,
  device = "pdf",
  width = 6.5, 
  height = 5
)
