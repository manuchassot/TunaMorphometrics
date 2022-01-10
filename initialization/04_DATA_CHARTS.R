

RAW_SAMPLES_WITH_ENVIRONMENT[, STOCK := paste(ocean_code, species_code_fao, sep = " | ")]

ggplot(RAW_SAMPLES_WITH_ENVIRONMENT, aes(x = fork_length, y = whole_fish_weight)) +
  geom_point(shape = 3, size= 0.8) +
  theme_bw() +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)") +
  facet_wrap(~STOCK, scales = "free")
