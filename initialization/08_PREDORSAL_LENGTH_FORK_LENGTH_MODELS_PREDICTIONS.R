
# PREDORSAL LENGTH - FORK LENGTH

FDL_FL_PREDICTIONs_FUNCTION = function(Dataset, OceanCode = "AO", SpeciesCode = "BET", FinalModel){
  
  InputDataSet = Dataset[ocean_code == OceanCode & species_code_fao == SpeciesCode & !is.na(first_dorsal_length) & !is.na(fork_length)]
  
  # Data frame for predictions
  
  PredictionsDF = data.table(ocean_code = OceanCode, species_code_fao = SpeciesCode, first_dorsal_length = seq(floor(min(InputDataSet$first_dorsal_length, na.rm = TRUE)), ceiling(max(InputDataSet$first_dorsal_length, na.rm = TRUE)), 0.01))
  
  PredictionsDF[, fork_length := coef(FinalModel)[1] + first_dorsal_length * coef(FinalModel)[2]]
  
  return(list(DATA = InputDataSet, PREDICTIONS = PredictionsDF))
}

# Indian Ocean | Skipjack tuna ####

LM_FDL_FL_IO_SKJ = FDL_FL_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "IO", "SKJ", SKJ_FDL_FL_LM_IO_FINAL)

PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_SKJ =
  ggplot(LM_FDL_FL_IO_SKJ[["DATA"]], aes(x = first_dorsal_length, y = fork_length)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "SKJ", FILL]) +
  theme_bw() +
  geom_line(data = LM_FDL_FL_IO_SKJ[["PREDICTIONS"]], aes(x = first_dorsal_length, y = fork_length), color = SPECIES_COL_SHAPE[species_code_fao == "SKJ", OUTLINE], size = 1.2) +
#  scale_x_continuous(limits = c(25, 73)) +
#  scale_y_continuous(limits = c(0, 10.5)) +
  labs(x = "Pre-dorsal length (cm)", y = "Fork length (cm)", title = "Skipjack tuna | Indian Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_SKJ.png", PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_SKJ, width = 6, height = 4.5)

# Indian Ocean | Bigeye tuna ####

LM_FDL_FL_IO_BET = FDL_FL_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "IO", "BET", BET_FDL_FL_LM_IO_FINAL)

PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_BET =
  ggplot(LM_FDL_FL_IO_BET[["DATA"]], aes(x = first_dorsal_length, y = fork_length)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "BET", FILL]) +
  theme_bw() +
  geom_line(data = LM_FDL_FL_IO_BET[["PREDICTIONS"]], aes(x = first_dorsal_length, y = fork_length), color = SPECIES_COL_SHAPE[species_code_fao == "BET", OUTLINE], size = 1.2) +
#  scale_y_continuous(limits = c(0, 125)) +
  labs(x = "Pre-dorsal length (cm)", y = "Fork length (cm)", title = "Bigeye tuna | Indian Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_BET.png", PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_BET, width = 6, height = 4.5)

# Indian Ocean | Yellowfin tuna ####

LM_FDL_FL_IO_YFT = FDL_FL_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "IO", "YFT", YFT_FDL_FL_LM_IO_FINAL)

PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_YFT =
  ggplot(LM_FDL_FL_IO_YFT[["DATA"]], aes(x = first_dorsal_length, y = fork_length)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "YFT", FILL]) +
  theme_bw() +
  geom_line(data = LM_FDL_FL_IO_YFT[["PREDICTIONS"]], aes(x = first_dorsal_length, y = fork_length), color = SPECIES_COL_SHAPE[species_code_fao == "YFT", OUTLINE], size = 1.2) +
#  scale_x_continuous(limits = c(29, 178)) +
#  scale_y_continuous(limits = c(0, 119)) +
  labs(x = "Pre-dorsal length (cm)", y = "Fork length (cm)", title = "Yellowfin tuna | Indian Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_YFT.png", PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_YFT, width = 6, height = 4.5)

# Atlantic Ocean | Skipjack tuna ####

LM_FDL_FL_AO_SKJ = FDL_FL_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "AO", "SKJ", SKJ_FDL_FL_LM_AO_FINAL)

PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_SKJ =
  ggplot(LM_FDL_FL_AO_SKJ[["DATA"]], aes(x = first_dorsal_length, y = fork_length)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "SKJ", FILL]) +
  theme_bw() +
  geom_line(data = LM_FDL_FL_AO_SKJ[["PREDICTIONS"]], aes(x = first_dorsal_length, y = fork_length), color = SPECIES_COL_SHAPE[species_code_fao == "SKJ", OUTLINE], size = 1.2) +
#  scale_x_continuous(limits = c(25, 73)) +
#  scale_y_continuous(limits = c(0, 10.5)) +
  labs(x = "Pre-dorsal length (cm)", y = "Fork length (cm)", title = "Skipjack tuna | Atlantic Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_SKJ.png", PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_SKJ, width = 6, height = 4.5)

# Atlantic Ocean | Bigeye tuna ####

LM_FDL_FL_AO_BET = FDL_FL_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "AO", "BET", BET_FDL_FL_LM_AO_FINAL)

PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_BET =
  ggplot(LM_FDL_FL_AO_BET[["DATA"]], aes(x = first_dorsal_length, y = fork_length)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "BET", FILL]) +
  theme_bw() +
  geom_line(data = LM_FDL_FL_AO_BET[["PREDICTIONS"]], aes(x = first_dorsal_length, y = fork_length), color = SPECIES_COL_SHAPE[species_code_fao == "BET", OUTLINE], size = 1.2) +
  labs(x = "Pre-dorsal length (cm)", y = "Fork length (cm)", title = "Bigeye tuna | Atlantic Ocean") +
#  scale_y_continuous(limits = c(0, 125)) +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_BET.png", PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_BET, width = 6, height = 4.5)

# Atlantic Ocean | Yellowfin tuna ####

LM_FDL_FL_AO_YFT = FDL_FL_PREDICTIONs_FUNCTION(TUNA_SAMPLES, "AO", "YFT", YFT_FDL_FL_LM_AO_FINAL)

PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_YFT =
  ggplot(LM_FDL_FL_AO_YFT[["DATA"]], aes(x = first_dorsal_length, y = fork_length)) +
  geom_point(shape = 21, size = 1, color = SPECIES_COL_SHAPE[species_code_fao == "YFT", FILL]) +
  theme_bw() +
  geom_line(data = LM_FDL_FL_AO_YFT[["PREDICTIONS"]], aes(x = first_dorsal_length, y = fork_length), color = SPECIES_COL_SHAPE[species_code_fao == "YFT", OUTLINE], size = 1.2) +
#  scale_x_continuous(limits = c(29, 178)) +
#  scale_y_continuous(limits = c(0, 119)) +
  labs(x = "Pre-dorsal length (cm)", y = "Fork length (cm)", title = "Yellowfin tuna | Atlantic Ocean") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none")

ggsave("../outputs/charts/FITS/PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_YFT.png", PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_YFT, width = 6, height = 4.5)

LM_FDL_FL_ALL_FITS = PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_SKJ + PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_YFT + PREDORSAL_LENGTH_FORK_LENGTH_FIT_AO_BET + PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_SKJ + PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_YFT + PREDORSAL_LENGTH_FORK_LENGTH_FIT_IO_BET + plot_layout(ncol = 3, nrow = 2)

ggsave("../outputs/charts/FITS/LM_FDL_FL_ALL_FITS.png", LM_FDL_FL_ALL_FITS, width = 16, height = 9)
