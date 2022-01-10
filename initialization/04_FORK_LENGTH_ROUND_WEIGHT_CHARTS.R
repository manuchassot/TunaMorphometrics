
# SPECIES COLORS
SPECIES_COL    = data.table(species_code_fao = c("BET", "SKJ", "YFT"), FILL = pal_jco(alpha = 0.6)(3), OUTLINE = darken(pal_jco(alpha = 0.6)(3), 0.2))

# Fork length - round weight relationships: all data sets included ####

FORK_LENGTH_ROUND_WEIGHT_FULL_DATA_SET_SCATTERPLOT =
  ggplot(FULL_DATASET[!is.na(fork_length) & !is.na(whole_fish_weight)], aes(x = fork_length, y = whole_fish_weight, color = species_code_fao)) +
  geom_point(shape = 3, size= 0.8) +
  theme_bw() +
  scale_color_manual(values = SPECIES_COL$FILL) +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)") +
  facet_wrap(~STOCK, scales = "free") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

save_plot("../outputs/charts/FL_RW/FORK_LENGTH_ROUND_WEIGHT_FULL_DATA_SET_SCATTERPLOT.png", FORK_LENGTH_ROUND_WEIGHT_FULL_DATA_SET_SCATTERPLOT, 10, 4.5/8*10)

# Fork length - round weight: BET ###

FORK_LENGTH_ROUND_WEIGHT_FULL_DATA_SET_BET_SCATTERPLOT =
ggplot(FULL_DATASET[!is.na(fork_length) & !is.na(whole_fish_weight) & species_code_fao == "BET"], aes(x = fork_length, y = whole_fish_weight, color = ocean_code)) +
  geom_point(shape = 3, size= 0.8) +
  theme_bw() +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "bottom", legend.title = element_blank())
