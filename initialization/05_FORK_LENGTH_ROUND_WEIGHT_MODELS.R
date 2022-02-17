
TUNA_SAMPLES = 
  SAMPLES_WITH_ENVIRONMENT_SF %>% 
  cbind(., st_coordinates(.)) %>%
  st_set_geometry(NULL) %>%
  setDT()
  
# Prepare the data
TUNA_SAMPLES[, log10FL  := log(fork_length, 10)]
TUNA_SAMPLES[, log10RW  := log(whole_fish_weight, 10)]
TUNA_SAMPLES[, quarter  := factor(fishing_quarter)]
TUNA_SAMPLES[, ProvCode := factor(ProvCode)]

# BET models ####

# Define data set
BET_SAMPLES = TUNA_SAMPLES[species_code_fao == "BET" & ProvCode %in% c("GUIN", "ETRA", "MONS", "WTRA")]

# Model with sex (removing immature fish)
BET_LM_FULL_SEX = lm(log10RW ~ log10FL + sex + quarter + ProvCode, data = BET_SAMPLES[sex %in% c("M", "F")])

#stepAIC(BET_LM_FULL_SEX)
#plot(BET_LM_FULL_SEX)
#anova(BET_LM_FULL_SEX)
#summary(BET_LM_FULL_SEX)

# Final model
BET_LM_FULL = lm(log10RW ~ log10FL + quarter + ProvCode, data = BET_SAMPLES)

stepAIC(BET_LM_FULL)
plot(BET_LM_FULL)
anova(BET_LM_FULL)
summary(BET_LM_FULL)

# YFT models ####

# Define data set
YFT_SAMPLES = TUNA_SAMPLES[species_code_fao == "YFT" & ProvCode %in% c("GUIN", "ETRA", "MONS", "WTRA", "ARAB")]

# Model with sex (removing immature fish)
YFT_LM_FULL_SEX = lm(log10RW ~ log10FL + sex + quarter + ProvCode, data = YFT_SAMPLES[sex %in% c("M", "F")])

#stepAIC(YFT_LM_FULL_SEX)
#plot(YFT_LM_FULL_SEX)
#anova(YFT_LM_FULL_SEX)
#summary(YFT_LM_FULL_SEX)

# Final model
YFT_LM_FULL = lm(log10RW ~ log10FL + quarter + ProvCode, data = YFT_SAMPLES)

stepAIC(BET_LM_FULL)
plot(BET_LM_FULL)
anova(BET_LM_FULL)
summary(BET_LM_FULL)

# SKJ models ####

# Define data set
SKJ_SAMPLES = TUNA_SAMPLES[species_code_fao == "SKJ" & ProvCode %in% c("CNRY", "GUIN", "ETRA", "MONS", "WTRA")]

# Model with measuring device
SKJ_LM_FULL_DEVICE = lm(log10RW ~ log10FL + measuring_device_1 + quarter, data = SKJ_SAMPLES[ProvCode %in% "ETRA"])


# Model with sex (removing immature fish)
SKJ_LM_FULL_SEX = lm(log10RW ~ log10FL + sex + quarter + ProvCode, data = SKJ_SAMPLES[sex %in% c("M", "F")])

#stepAIC(SKJ_LM_FULL_SEX)
#plot(SKJ_LM_FULL_SEX)
#anova(SKJ_LM_FULL_SEX)
#summary(SKJ_LM_FULL_SEX)

# Final model
SKJ_LM_FULL = lm(log10RW ~ log10FL + sex + quarter + ProvCode, data = SKJ_SAMPLES)

plot(SKJ_LM_FULL)
anova(SKJ_LM_FULL)
summary(SKJ_LM_FULL)


