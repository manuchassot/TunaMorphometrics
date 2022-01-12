
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

ggplot(BET_FORK_LENGTH_ROUND_WEIGHT_DATASET, aes(x = fork_length, y = whole_fish_weight)) +
  geom_point(shape = 3, size= 0.8, color = "lightblue") +
  theme_bw() +
  geom_line(data = IO_BET_FL_RW_PREDICTION, aes(x = fork_length, y = whole_fish_weight_predicted), size = 1.2, color = "#0073C299") +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")
