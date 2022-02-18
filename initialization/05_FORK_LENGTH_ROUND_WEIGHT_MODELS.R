
# Prepare the data
TUNA_SAMPLES[, log10FL  := log(fork_length, 10)]
TUNA_SAMPLES[, log10RW  := log(whole_weight_kg, 10)]
TUNA_SAMPLES[, quarter  := factor(capture_quarter)]
TUNA_SAMPLES[, ProvCode := factor(ProvCode)]
TUNA_SAMPLES[, aggregation := factor(aggregation)]

# BIGEYE TUNA ####

# Geo-referenced data set 
BET_SAMPLES = TUNA_SAMPLES[species_code_fao == "BET" & ProvCode %in% c("MONS", "ETRA", "GUIN", "WTRA")]

# Full model with covariates
BET_FL_RD_LM_FULL = lm(log10RW ~ log10FL + ProvCode  + quarter + aggregation + sex, data = BET_SAMPLES[sex %in% c("M", "F") & aggregation %in% c("DFAD", "FSC")])

# stepAIC(BET_FL_RD_LM_FULL)
# covariates aggregation and sex are not kept in the model

# Final model
BET_FL_RD_FULL_FINAL = lm(log10RW ~ log10FL + ProvCode + quarter, data = BET_SAMPLES)

ANOVA_BET_FL_RD_FULL = anova_table("Bigeye tuna", BET_FL_RD_FULL_FINAL)
R2_BET_FL_RD_FULL    = (summary(BET_FL_RD_FULL_FINAL))$r.squared

# Model without covariate
BET_FL_RD_FINAL = lm(log10RW ~ log10FL, data = TUNA_SAMPLES[species_code_fao == "BET"])

ANOVA_BET_FL_RD = anova_table("Bigeye tuna", BET_FL_RD_FINAL)
R2_BET_FL_RD    = (summary(BET_FL_RD_FINAL))$r.squared

# YELLOWFIN TUNA ####

# Geo-referenced data set
YFT_SAMPLES = TUNA_SAMPLES[species_code_fao == "YFT" & ProvCode %in% c("MONS", "ETRA", "EAFR", "GUIN", "ARAB", "WTRA")]

# Full model with covariates
YFT_FL_RD_LM_FULL = lm(log10RW ~ log10FL + ProvCode + quarter + aggregation + sex, data = YFT_SAMPLES[sex %in% c("M", "F") & aggregation %in% c("DFAD", "FSC")])

#stepAIC(YFT_FL_RD_LM_FULL)
# all covariates kept in the model

# Final model
YFT_FL_RD_FULL_FINAL = YFT_FL_RD_LM_FULL

ANOVA_YFT_FL_RD_FULL = anova_table("Yellowfin tuna", YFT_FL_RD_FULL_FINAL)
R2_YFT_FL_RD_FULL    = (summary(YFT_FL_RD_FULL_FINAL))$r.squared

# Model without covariate
YFT_FL_RD_FINAL = lm(log10RW ~ log10FL, data = TUNA_SAMPLES[species_code_fao == "YFT"])

ANOVA_YFT_FL_RD = anova_table("Yellowfin tuna", YFT_FL_RD_FINAL)
R2_YFT_FL_RD    = (summary(YFT_FL_RD_FINAL))$r.squared

# SKIPJACK TUNA ####

# Geo-referenced data set
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


