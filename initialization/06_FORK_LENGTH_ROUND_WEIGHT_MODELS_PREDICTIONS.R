
# FORK LENGTH - ROUND WEIGHT

LM_FL_RW = function(Dataset, OceanCode, SpeciesCode){

# Data frame for predictions
FL_RW_PREDICTION = data.table(fork_length = seq(floor(min(FORK_LENGTH_ROUND_WEIGHT_DATASET$fork_length)), ceiling(max(FORK_LENGTH_ROUND_WEIGHT_DATASET$fork_length)), 0.1))

FL_RW_PREDICTION[, whole_fish_weight_predicted_BIASED := a_LM_FL_RW * fork_length ^ b_LM_FL_RW]
FL_RW_PREDICTION[, whole_fish_weight_predicted_NEYMAN := a_LM_FL_RW_NEYMAN * fork_length ^ b_LM_FL_RW]
FL_RW_PREDICTION[, whole_fish_weight_predicted_SMITH  := a_LM_FL_RW_SMITH * fork_length ^ b_LM_FL_RW]

return(list(MODEL = LM_FORK_LENGTH_ROUND_WEIGHT, a_LM_FL_RW = a_LM_FL_RW, a_LM_FL_RW_NEYMAN = a_LM_FL_RW_NEYMAN, a_LM_FL_RW_SMITH = a_LM_FL_RW_SMITH, b_LM_FL_RW = b_LM_FL_RW, DATA = FORK_LENGTH_ROUND_WEIGHT_DATASET, PREDICTIONS = FL_RW_PREDICTION))
}

# Indian Ocean | Bigeye tuna ####

LM_FL_RW_IO_BET = LM_FL_RW(FULL_DATASET, "IO", "BET")

FORK_LENGTH_ROUND_WEIGHT_FIT_IO_BET =
  ggplot(LM_FL_RW_IO_BET[["DATA"]], aes(x = fork_length, y = whole_fish_weight)) +
  geom_point(shape = 3, size = 0.8, color = "lightblue") +
  theme_bw() +
#  geom_line(data = LM_FL_RW_IO_BET[["PREDICTIONS"]], aes(x = fork_length, y = whole_fish_weight_predicted_BIASED), color = "green") +
#  geom_line(data = LM_FL_RW_IO_BET[["PREDICTIONS"]], aes(x = fork_length, y = whole_fish_weight_predicted_NEYMAN), color = "red") +
  geom_line(data = LM_FL_RW_IO_BET[["PREDICTIONS"]], aes(x = fork_length, y = whole_fish_weight_predicted_SMITH), color = "darkblue") +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)", title = "Bigeye tuna | Indian Ocean") +
theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

# anova(LM_FL_RW_IO_SKJ["MODEL"]$MODEL)
# summary(LM_FL_RW_IO_SKJ["MODEL"]$MODEL)
# plot(LM_FL_RW_IO_SKJ["MODEL"]$MODEL)

ggsave("../outputs/charts/FREQUENTIST/FORK_LENGTH_ROUND_WEIGHT_FIT_IO_BET.png", FORK_LENGTH_ROUND_WEIGHT_FIT_IO_BET, width = 8, height = 4.5)

# Indian Ocean | Skipjack tuna ####

LM_FL_RW_IO_SKJ = LM_FL_RW(FULL_DATASET, "IO", "SKJ")

FORK_LENGTH_ROUND_WEIGHT_FIT_IO_SKJ =
  ggplot(LM_FL_RW_IO_SKJ[["DATA"]], aes(x = fork_length, y = whole_fish_weight)) +
  geom_point(shape = 3, size = 0.8, color = "red") +
  theme_bw() +
  geom_line(data = LM_FL_RW_IO_SKJ[["PREDICTIONS"]], aes(x = fork_length, y = whole_fish_weight_predicted_SMITH), color = "red", size = 1.2) +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)", title = "Skipjack tuna | Indian Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FREQUENTIST/FORK_LENGTH_ROUND_WEIGHT_FIT_IO_SKJ.png", FORK_LENGTH_ROUND_WEIGHT_FIT_IO_SKJ, width = 8, height = 4.5)

# Indian Ocean | Yellowfin tuna ####

LM_FL_RW_IO_YFT = LM_FL_RW(FULL_DATASET, "IO", "YFT")

FORK_LENGTH_ROUND_WEIGHT_FIT_IO_YFT =
  ggplot(LM_FL_RW_IO_YFT[["DATA"]], aes(x = fork_length, y = whole_fish_weight)) +
  geom_point(shape = 3, size = 0.8, color = "orange") +
  theme_bw() +
  geom_line(data = LM_FL_RW_IO_YFT[["PREDICTIONS"]], aes(x = fork_length, y = whole_fish_weight_predicted_SMITH), color = "black", size = 1.2) +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)", title = "Yellowfin tuna | Indian Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FREQUENTIST/FORK_LENGTH_ROUND_WEIGHT_FIT_IO_YFT.png", FORK_LENGTH_ROUND_WEIGHT_FIT_IO_YFT, width = 8, height = 4.5)

# Atlantic Ocean | Bigeye tuna

LM_FL_RW_AO_BET = LM_FL_RW(FULL_DATASET, "AO", "BET")

FORK_LENGTH_ROUND_WEIGHT_FIT_AO_BET =
  ggplot(LM_FL_RW_AO_BET[["DATA"]], aes(x = fork_length, y = whole_fish_weight)) +
  geom_point(shape = 3, size = 0.8, color = "lightblue") +
  theme_bw() +
  geom_line(data = LM_FL_RW_AO_BET[["PREDICTIONS"]], aes(x = fork_length, y = whole_fish_weight_predicted_SMITH), color = "darkblue", size = 1.2) +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)", title = "Bigeye tuna | Atlantic Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FREQUENTIST/FORK_LENGTH_ROUND_WEIGHT_FIT_AO_BET.png", FORK_LENGTH_ROUND_WEIGHT_FIT_AO_BET, width = 8, height = 4.5)

# Atlantic Ocean | Skipjack tuna

LM_FL_RW_AO_SKJ = LM_FL_RW(FULL_DATASET, "AO", "SKJ")

FORK_LENGTH_ROUND_WEIGHT_FIT_AO_SKJ =
  ggplot(LM_FL_RW_AO_SKJ[["DATA"]], aes(x = fork_length, y = whole_fish_weight)) +
  geom_point(shape = 3, size = 0.8, color = "red") +
  theme_bw() +
  geom_line(data = LM_FL_RW_AO_SKJ[["PREDICTIONS"]], aes(x = fork_length, y = whole_fish_weight_predicted_SMITH), color = "red", size = 1.2) +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)", title = "Skipjack tuna | Atlantic Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FREQUENTIST/FORK_LENGTH_ROUND_WEIGHT_FIT_AO_SKJ.png", FORK_LENGTH_ROUND_WEIGHT_FIT_AO_SKJ, width = 8, height = 4.5)

# Atlantic Ocean | Yellowfin tuna

LM_FL_RW_AO_YFT = LM_FL_RW(FULL_DATASET, "AO", "YFT")

FORK_LENGTH_ROUND_WEIGHT_FIT_AO_YFT =
  ggplot(LM_FL_RW_AO_YFT[["DATA"]], aes(x = fork_length, y = whole_fish_weight)) +
  geom_point(shape = 3, size = 0.8, color = "orange") +
  theme_bw() +
  geom_line(data = LM_FL_RW_AO_YFT[["PREDICTIONS"]], aes(x = fork_length, y = whole_fish_weight_predicted_SMITH), color = "black", size = 1.2) +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)", title = "Yellowfin tuna | Atlantic Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FREQUENTIST/FORK_LENGTH_ROUND_WEIGHT_FIT_AO_YFT.png", FORK_LENGTH_ROUND_WEIGHT_FIT_AO_YFT, width = 8, height = 4.5)

ALL_FITS_FREQUENTIST = FORK_LENGTH_ROUND_WEIGHT_FIT_AO_BET + FORK_LENGTH_ROUND_WEIGHT_FIT_AO_SKJ + FORK_LENGTH_ROUND_WEIGHT_FIT_AO_YFT + FORK_LENGTH_ROUND_WEIGHT_FIT_IO_BET + FORK_LENGTH_ROUND_WEIGHT_FIT_IO_SKJ + FORK_LENGTH_ROUND_WEIGHT_FIT_IO_YFT + plot_layout(ncol = 3, nrow = 2)

ggsave("../outputs/charts/FREQUENTIST/FORK_LENGTH_ROUND_WEIGHT_ALL_FITS_FREQUENTIST.png", ALL_FITS_FREQUENTIST, width = 16, height = 9)



