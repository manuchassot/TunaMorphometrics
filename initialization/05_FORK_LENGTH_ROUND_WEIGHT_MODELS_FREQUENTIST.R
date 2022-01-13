
# FORK LENGTH - ROUND WEIGHT

LM_FL_RW = function(Dataset, OceanCode, SpeciesCode){

  FORK_LENGTH_ROUND_WEIGHT_DATASET = Dataset[ocean_code == OceanCode & species_code_fao == SpeciesCode & !is.na(fork_length) & !is.na(whole_fish_weight)]

  SpeciesEnglish    = unique(FORK_LENGTH_ROUND_WEIGHT_DATASET$species_english_name)
  SpeciesScientific = unique(FORK_LENGTH_ROUND_WEIGHT_DATASET$species_scientific_name)
  Ocean             = unique(FORK_LENGTH_ROUND_WEIGHT_DATASET$ocean)
  Color             = as.factor(SPECIES_COL[species_code_fao == SpeciesCode, FILL])  
  LM_FORK_LENGTH_ROUND_WEIGHT = lm(log10(whole_fish_weight) ~ log10(fork_length), data = FORK_LENGTH_ROUND_WEIGHT_DATASET)

  a_LM_FL_RW <- 10^(coefficients(LM_FORK_LENGTH_ROUND_WEIGHT)[1]) * 10^(var(residuals(LM_FORK_LENGTH_ROUND_WEIGHT))/2)

  b_LM_FL_RW = coefficients(LM_FORK_LENGTH_ROUND_WEIGHT)[2]

  FORK_LENGTH_ROUND_WEIGHT_DATASET[, LOG_ROUND_WEIGHT := log10(whole_fish_weight)]
  FORK_LENGTH_ROUND_WEIGHT_DATASET[, LOG_ROUND_WEIGHT_PREDICTED := predict.lm(LM_FORK_LENGTH_ROUND_WEIGHT, se.fit = T)$fit]
  FORK_LENGTH_ROUND_WEIGHT_DATASET[, ROUND_WEIGHT_PREDICTED := 10^(LOG_ROUND_WEIGHT_PREDICTED)]
  FORK_LENGTH_ROUND_WEIGHT_DATASET[, ROUND_WEIGHT_PREDICTED_BIAS :=  a_LM_FL_RW*fork_length^b_LM_FL_RW]

# Data frame for predictions
FL_RW_PREDICTION = data.table(fork_length = seq(floor(min(FORK_LENGTH_ROUND_WEIGHT_DATASET$fork_length)), ceiling(max(FORK_LENGTH_ROUND_WEIGHT_DATASET$fork_length)), 0.1))

FL_RW_PREDICTION[, whole_fish_weight_predicted := a_LM_FL_RW * fork_length ^ b_LM_FL_RW]

FORK_LENGTH_ROUND_WEIGHT_FIT =
ggplot(FORK_LENGTH_ROUND_WEIGHT_DATASET, aes(x = fork_length, y = whole_fish_weight)) +
  geom_point(shape = 3, size = 0.8, color = colortest) +
#  scale_color_identity() +
  theme_bw() +
  geom_line(data = FL_RW_PREDICTION, aes(x = fork_length, y = whole_fish_weight_predicted, color = Color), size = 1.2) +
#  scale_color_identity() +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)", title = paste(SpeciesEnglish, Ocean, sep = " | ")) +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

print(FORK_LENGTH_ROUND_WEIGHT_FIT)

ggsave(paste0("../outputs/charts/FL_RW/FORK_LENGTH_ROUND_WEIGHT_FIT_", OceanCode, "_", SpeciesCode, ".png"), FORK_LENGTH_ROUND_WEIGHT_FIT, width = 8, height = 4.5)

return(LM_FORK_LENGTH_ROUND_WEIGHT)
}

##########################################################################################################################

# FORK LENGTH - ROUND WEIGHT: BET

IO_BET_FORK_LENGTH_ROUND_WEIGHT_DATASET = FULL_DATASET[species_code_fao == "BET" & ocean_code == "IO" & !is.na(fork_length) & !is.na(whole_fish_weight)]

# LOG-NORMAL ERROR

LM_IO_BET_FORK_LENGTH_ROUND_WEIGHT = lm(log10(whole_fish_weight) ~ log10(fork_length), data = IO_BET_FORK_LENGTH_ROUND_WEIGHT_DATASET)

a_LM_FL_RW_BET_IO <- 10^(coefficients(LM_IO_BET_FORK_LENGTH_ROUND_WEIGHT)[1]) * 10^(var(residuals(LM_IO_BET_FORK_LENGTH_ROUND_WEIGHT))/2)

b_LM_FL_RW_BET_IO = coefficients(LM_IO_BET_FORK_LENGTH_ROUND_WEIGHT)[2]

IO_BET_FORK_LENGTH_ROUND_WEIGHT_DATASET[, LOG_ROUND_WEIGHT := log10(whole_fish_weight)]
IO_BET_FORK_LENGTH_ROUND_WEIGHT_DATASET[, LOG_ROUND_WEIGHT_PREDICTED := predict.lm(LM_IO_BET_FORK_LENGTH_ROUND_WEIGHT, se.fit = T)$fit]
IO_BET_FORK_LENGTH_ROUND_WEIGHT_DATASET[, ROUND_WEIGHT_PREDICTED := 10^(LOG_ROUND_WEIGHT_PREDICTED)]
IO_BET_FORK_LENGTH_ROUND_WEIGHT_DATASET[, ROUND_WEIGHT_PREDICTED_BIAS :=  a_LM_FL_RW_BET_IO*fork_length^b_LM_FL_RW_BET_IO]

# Data frame for predictions
IO_BET_FL_RW_PREDICTION = data.table(fork_length = seq(28, 174, 0.1))
IO_BET_FL_RW_PREDICTION[, whole_fish_weight_predicted := a_LM_FL_RW_BET_IO*fork_length^b_LM_FL_RW_BET_IO]

ggplot(BET_FORK_LENGTH_ROUND_WEIGHT_DATASET, aes(x = fork_length, y = whole_fish_weight), color = "#0073C299") +
  geom_point(shape = 3, size = 0.8) +
  theme_bw() +
  scale_color_identity() +
geom_line(data = IO_BET_FL_RW_PREDICTION, aes(x = fork_length, y = whole_fish_weight_predicted, color = "#0073C299"), size = 1.2) +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")
