
# FORK LENGTH - ROUND WEIGHT

LM_FL_RW = function(Dataset, OceanCode, SpeciesCode){

  # Data
  FORK_LENGTH_ROUND_WEIGHT_DATASET = Dataset[ocean_code == OceanCode & species_code_fao == SpeciesCode & !is.na(fork_length) & !is.na(whole_fish_weight)]
  SpeciesEnglish    = unique(FORK_LENGTH_ROUND_WEIGHT_DATASET$species_english_name)
  SpeciesScientific = unique(FORK_LENGTH_ROUND_WEIGHT_DATASET$species_scientific_name)
  Ocean             = unique(FORK_LENGTH_ROUND_WEIGHT_DATASET$ocean)
  Color             = as.factor(SPECIES_COL[species_code_fao == SpeciesCode, FILL])  
  
  # Model
  LM_FORK_LENGTH_ROUND_WEIGHT = lm(log10(whole_fish_weight) ~ log10(fork_length), data = FORK_LENGTH_ROUND_WEIGHT_DATASET)

  # Inference
  a_LM_FL_RW <- 10^(coefficients(LM_FORK_LENGTH_ROUND_WEIGHT)[1]) * 10^(var(residuals(LM_FORK_LENGTH_ROUND_WEIGHT))/2)
  b_LM_FL_RW = coefficients(LM_FORK_LENGTH_ROUND_WEIGHT)[2]

  # Predictions
  FORK_LENGTH_ROUND_WEIGHT_DATASET[, LOG_ROUND_WEIGHT := log10(whole_fish_weight)]
  FORK_LENGTH_ROUND_WEIGHT_DATASET[, LOG_ROUND_WEIGHT_PREDICTED := predict.lm(LM_FORK_LENGTH_ROUND_WEIGHT, se.fit = T)$fit]
  FORK_LENGTH_ROUND_WEIGHT_DATASET[, ROUND_WEIGHT_PREDICTED := 10^(LOG_ROUND_WEIGHT_PREDICTED)]
  FORK_LENGTH_ROUND_WEIGHT_DATASET[, ROUND_WEIGHT_PREDICTED_BIAS_CORRECTED :=  a_LM_FL_RW*fork_length^b_LM_FL_RW]

# Data frame for predictions
FL_RW_PREDICTION = data.table(fork_length = seq(floor(min(FORK_LENGTH_ROUND_WEIGHT_DATASET$fork_length)), ceiling(max(FORK_LENGTH_ROUND_WEIGHT_DATASET$fork_length)), 0.1))

FL_RW_PREDICTION[, whole_fish_weight_predicted := a_LM_FL_RW * fork_length ^ b_LM_FL_RW]

return(list(LM_FORK_LENGTH_ROUND_WEIGHT, FORK_LENGTH_ROUND_WEIGHT_DATASET, FL_RW_PREDICTION))
}

LM_FL_RW_IO_BET = LM_FL_RW(FULL_DATASET, "IO", "BET")

FORK_LENGTH_ROUND_WEIGHT_FIT =
  ggplot(LM_FL_RW_IO_BET[[2]], aes(x = fork_length, y = whole_fish_weight)) +
  geom_point(shape = 3, size = 0.8, color = "lightblue") +
  theme_bw() +
  geom_line(data = LM_FL_RW_IO_BET[[3]], aes(x = fork_length, y = whole_fish_weight_predicted), color = "darkblue") +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)", title = "Bigeye tuna | Indian Ocean") +
theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FREQUENTIST/FORK_LENGTH_ROUND_WEIGHT_FIT_IO_BET.png", FORK_LENGTH_ROUND_WEIGHT_FIT, width = 8, height = 4.5)





