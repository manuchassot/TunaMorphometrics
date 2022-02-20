
# FORK LENGTH - ROUND WEIGHT

FL_RD_PREDICTIONs_FUNCTION = function(Dataset, OceanCode = "AO", SpeciesCode = "BET", FinalModel){
  
  InputDataSet = Dataset[ocean_code == OceanCode & species_code_fao == SpeciesCode & !is.na(fork_length) & !is.na(whole_weight_kg)]
  
  # Data frame for predictions
  
  PredictionsDF = data.table(ocean_code = OceanCode, species_code_fao = SpeciesCode, fork_length = seq(floor(min(InputDataSet$fork_length, na.rm = TRUE)), ceiling(max(InputDataSet$fork_length, na.rm = TRUE)), 0.1))
  
  PredictionsDF[, whole_weight_kg := 10^coef(FinalModel)[1]*exp(var(residuals(FinalModel))*2.651) * fork_length ^ coef(FinalModel)[2]]
  
  return(list(DATA = InputDataSet, PREDICTIONS = PredictionsDF))
}

# Indian Ocean | Skipjack tuna ####

LM_FL_RW_IO_SKJ = FL_RD_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "IO", "SKJ", SKJ_FL_RD_LM_IO_FINAL)

FORK_LENGTH_ROUND_WEIGHT_FIT_IO_SKJ =
  ggplot(LM_FL_RW_IO_SKJ[["DATA"]], aes(x = fork_length, y = whole_weight_kg)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "SKJ", FILL]) +
  theme_bw() +
  geom_line(data = LM_FL_RW_IO_SKJ[["PREDICTIONS"]], aes(x = fork_length, y = whole_weight_kg), color = SPECIES_COL_SHAPE[species_code_fao == "SKJ", OUTLINE], size = 1.2) +
  scale_x_continuous(limits = c(25, 73)) +
  scale_y_continuous(limits = c(0, 10.5)) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)", title = "Skipjack tuna | Indian Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/FORK_LENGTH_ROUND_WEIGHT_FIT_IO_SKJ.png", FORK_LENGTH_ROUND_WEIGHT_FIT_IO_SKJ, width = 6, height = 4.5)

# Indian Ocean | Bigeye tuna ####

LM_FL_RW_IO_BET = FL_RD_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "IO", "BET", BET_FL_RD_LM_IO_FINAL)

FORK_LENGTH_ROUND_WEIGHT_FIT_IO_BET =
  ggplot(LM_FL_RW_IO_BET[["DATA"]], aes(x = fork_length, y = whole_weight_kg)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "BET", FILL]) +
  theme_bw() +
  geom_line(data = LM_FL_RW_IO_BET[["PREDICTIONS"]], aes(x = fork_length, y = whole_weight_kg), color = SPECIES_COL_SHAPE[species_code_fao == "BET", OUTLINE], size = 1.2) +
  scale_y_continuous(limits = c(0, 125)) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)", title = "Bigeye tuna | Indian Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/FORK_LENGTH_ROUND_WEIGHT_FIT_IO_BET.png", FORK_LENGTH_ROUND_WEIGHT_FIT_IO_BET, width = 6, height = 4.5)

# Indian Ocean | Yellowfin tuna ####

LM_FL_RW_IO_YFT = FL_RD_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "IO", "YFT", YFT_FL_RD_LM_IO_FINAL)

FORK_LENGTH_ROUND_WEIGHT_FIT_IO_YFT =
  ggplot(LM_FL_RW_IO_YFT[["DATA"]], aes(x = fork_length, y = whole_weight_kg)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "YFT", FILL]) +
  theme_bw() +
  geom_line(data = LM_FL_RW_IO_YFT[["PREDICTIONS"]], aes(x = fork_length, y = whole_weight_kg), color = SPECIES_COL_SHAPE[species_code_fao == "YFT", OUTLINE], size = 1.2) +
  scale_x_continuous(limits = c(29, 178)) +
  scale_y_continuous(limits = c(0, 119)) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)", title = "Yellowfin tuna | Indian Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/FORK_LENGTH_ROUND_WEIGHT_FIT_IO_YFT.png", FORK_LENGTH_ROUND_WEIGHT_FIT_IO_YFT, width = 6, height = 4.5)

# Atlantic Ocean | Skipjack tuna ####

LM_FL_RW_AO_SKJ = FL_RD_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "AO", "SKJ", SKJ_FL_RD_LM_AO_FINAL)

FORK_LENGTH_ROUND_WEIGHT_FIT_AO_SKJ =
  ggplot(LM_FL_RW_AO_SKJ[["DATA"]], aes(x = fork_length, y = whole_weight_kg)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "SKJ", FILL]) +
  theme_bw() +
  geom_line(data = LM_FL_RW_AO_SKJ[["PREDICTIONS"]], aes(x = fork_length, y = whole_weight_kg), color = SPECIES_COL_SHAPE[species_code_fao == "SKJ", OUTLINE], size = 1.2) +
  scale_x_continuous(limits = c(25, 73)) +
  scale_y_continuous(limits = c(0, 10.5)) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)", title = "Skipjack tuna | Atlantic Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/FORK_LENGTH_ROUND_WEIGHT_FIT_AO_SKJ.png", FORK_LENGTH_ROUND_WEIGHT_FIT_AO_SKJ, width = 6, height = 4.5)

# Atlantic Ocean | Bigeye tuna ####

LM_FL_RW_AO_BET = FL_RD_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "AO", "BET", BET_FL_RD_LM_AO_FINAL)

FORK_LENGTH_ROUND_WEIGHT_FIT_AO_BET =
  ggplot(LM_FL_RW_AO_BET[["DATA"]], aes(x = fork_length, y = whole_weight_kg)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "BET", FILL]) +
  theme_bw() +
  geom_line(data = LM_FL_RW_AO_BET[["PREDICTIONS"]], aes(x = fork_length, y = whole_weight_kg), color = SPECIES_COL_SHAPE[species_code_fao == "BET", OUTLINE], size = 1.2) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)", title = "Bigeye tuna | Atlantic Ocean") +
  scale_y_continuous(limits = c(0, 125)) +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/FORK_LENGTH_ROUND_WEIGHT_FIT_AO_BET.png", FORK_LENGTH_ROUND_WEIGHT_FIT_AO_BET, width = 6, height = 4.5)

# Atlantic Ocean | Yellowfin tuna ####

LM_FL_RW_AO_YFT = FL_RD_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "AO", "YFT", YFT_FL_RD_LM_AO_FINAL)

FORK_LENGTH_ROUND_WEIGHT_FIT_AO_YFT =
  ggplot(LM_FL_RW_AO_YFT[["DATA"]], aes(x = fork_length, y = whole_weight_kg)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "YFT", FILL]) +
  theme_bw() +
  geom_line(data = LM_FL_RW_AO_YFT[["PREDICTIONS"]], aes(x = fork_length, y = whole_weight_kg), color = SPECIES_COL_SHAPE[species_code_fao == "YFT", OUTLINE], size = 1.2) +
  scale_x_continuous(limits = c(29, 178)) +
  scale_y_continuous(limits = c(0, 119)) +
  labs(x = "Fork length (cm)", y = "Round weight (kg)", title = "Yellowfin tuna | Atlantic Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/FORK_LENGTH_ROUND_WEIGHT_FIT_AO_YFT.png", FORK_LENGTH_ROUND_WEIGHT_FIT_AO_YFT, width = 6, height = 4.5)

LM_FL_RW_ALL_FITS = FORK_LENGTH_ROUND_WEIGHT_FIT_AO_SKJ + FORK_LENGTH_ROUND_WEIGHT_FIT_AO_YFT + FORK_LENGTH_ROUND_WEIGHT_FIT_AO_BET + FORK_LENGTH_ROUND_WEIGHT_FIT_IO_SKJ + FORK_LENGTH_ROUND_WEIGHT_FIT_IO_YFT + FORK_LENGTH_ROUND_WEIGHT_FIT_IO_BET + plot_layout(ncol = 3, nrow = 2)

ggsave("../outputs/charts/FITS/LM_FL_RW_ALL_FITS.png", LM_FL_RW_ALL_FITS, width = 16, height = 9)



