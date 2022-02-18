
LM_FL_RW = function(Dataset, OceanCode, SpeciesCode){
  
  # Data
  FORK_LENGTH_ROUND_WEIGHT_DATASET = Dataset[ocean_code == OceanCode & species_code_fao == SpeciesCode & !is.na(fork_length) & !is.na(whole_fish_weight)]
  SpeciesEnglish    = unique(FORK_LENGTH_ROUND_WEIGHT_DATASET$species_english_name)
  SpeciesScientific = unique(FORK_LENGTH_ROUND_WEIGHT_DATASET$species_scientific_name)
  Ocean             = unique(FORK_LENGTH_ROUND_WEIGHT_DATASET$ocean)
  Color             = as.factor(SPECIES_COL_SHAPE[species_code_fao == SpeciesCode, FILL])  
  
  # Model
  LM_FORK_LENGTH_ROUND_WEIGHT = lm(log10(whole_fish_weight) ~ log10(fork_length), data = FORK_LENGTH_ROUND_WEIGHT_DATASET)
  
  # Inference
  a_LM_FL_RW        = 10^(coefficients(LM_FORK_LENGTH_ROUND_WEIGHT)[1]) 
  a_LM_FL_RW_NEYMAN = 10^(coefficients(LM_FORK_LENGTH_ROUND_WEIGHT)[1]) * 10^(var(residuals(LM_FORK_LENGTH_ROUND_WEIGHT))/2)
  a_LM_FL_RW_SMITH  = 10^(coefficients(LM_FORK_LENGTH_ROUND_WEIGHT)[1]) * exp(var(residuals(LM_FORK_LENGTH_ROUND_WEIGHT))*2.651)  # Smith 1993
  b_LM_FL_RW        = coefficients(LM_FORK_LENGTH_ROUND_WEIGHT)[2]
  
  # Predictions
  FORK_LENGTH_ROUND_WEIGHT_DATASET[, LOG_ROUND_WEIGHT := log10(whole_fish_weight)]
  FORK_LENGTH_ROUND_WEIGHT_DATASET[, LOG_ROUND_WEIGHT_PREDICTED := predict.lm(LM_FORK_LENGTH_ROUND_WEIGHT, se.fit = T)$fit]
  FORK_LENGTH_ROUND_WEIGHT_DATASET[, ROUND_WEIGHT_PREDICTED := 10^(LOG_ROUND_WEIGHT_PREDICTED)]
  FORK_LENGTH_ROUND_WEIGHT_DATASET[, ROUND_WEIGHT_PREDICTED_BIAS_CORRECTED :=  a_LM_FL_RW_SMITH*fork_length^b_LM_FL_RW]
  
  # Data frame for predictions
  FL_RW_PREDICTION = data.table(fork_length = seq(floor(min(FORK_LENGTH_ROUND_WEIGHT_DATASET$fork_length)), ceiling(max(FORK_LENGTH_ROUND_WEIGHT_DATASET$fork_length)), 0.1))
  
  FL_RW_PREDICTION[, whole_fish_weight_predicted_BIASED := a_LM_FL_RW * fork_length ^ b_LM_FL_RW]
  FL_RW_PREDICTION[, whole_fish_weight_predicted_NEYMAN := a_LM_FL_RW_NEYMAN * fork_length ^ b_LM_FL_RW]
  FL_RW_PREDICTION[, whole_fish_weight_predicted_SMITH  := a_LM_FL_RW_SMITH * fork_length ^ b_LM_FL_RW]
  
  return(list(MODEL = LM_FORK_LENGTH_ROUND_WEIGHT, a_LM_FL_RW = a_LM_FL_RW, a_LM_FL_RW_NEYMAN = a_LM_FL_RW_NEYMAN, a_LM_FL_RW_SMITH = a_LM_FL_RW_SMITH, b_LM_FL_RW = b_LM_FL_RW, DATA = FORK_LENGTH_ROUND_WEIGHT_DATASET, PREDICTIONS = FL_RW_PREDICTION))
}
