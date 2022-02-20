

# PREDORSAL LENGTH - ROUND WEIGHT ####

## BIGEYE TUNA ####

# Geo-referenced data set 
BET_SAMPLES = TUNA_SAMPLES[species_code_fao == "BET" & ProvCode %in% c("MONS", "ETRA", "GUIN", "WTRA")]

# Full model with covariates
BET_FDL_RD_LM_FULL = lm(log10RW ~ log10FDL + Province  + Quarter + SchoolType + Sex, data = BET_SAMPLES[sex %in% c("M", "F") & aggregation %in% c("DFAD", "FSC")])

# stepAIC(BET_FDL_RD_LM_FULL)
# all covariates are kept in the model

# Final model
BET_FDL_RD_LM_WITH_COV_FINAL = lm(log10RW ~ log10FDL + Province + Quarter + SchoolType + Sex, data = BET_SAMPLES)

BET_FDL_RD_LM_WITH_COV_FINAL_ANOVA = anova_table("Bigeye tuna", BET_FDL_RD_LM_WITH_COV_FINAL)
BET_FDL_RD_LM_WITH_COV_FINAL_R2    = (summary(BET_FDL_RD_LM_WITH_COV_FINAL))$r.squared

# Model without covariate
BET_FDL_RD_LM_AO_FINAL = lm(log10RW ~ log10FDL, data = TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "AO"])
BET_FDL_RD_LM_IO_FINAL = lm(log10RW ~ log10FDL, data = TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "IO"])

BET_FDL_RD_LM_AO_ANOVA = anova_table("Bigeye tuna", BET_FDL_RD_LM_AO_FINAL)
BET_FDL_RD_LM_AO_R2    = (summary(BET_FDL_RD_LM_AO_FINAL))$r.squared
a_BET_FDL_RD_AO        = 10^coef(BET_FDL_RD_LM_AO_FINAL)[1]*exp(var(residuals(BET_FDL_RD_LM_AO_FINAL))*2.651)
b_BET_FDL_RD_AO         = coef(BET_FDL_RD_LM_AO_FINAL)[2]

BET_FDL_RD_LM_IO_ANOVA = anova_table("Bigeye tuna", BET_FDL_RD_LM_IO_FINAL)
BET_FDL_RD_LM_IO_R2    = (summary(BET_FDL_RD_LM_IO_FINAL))$r.squared
a_BET_FDL_RD_IO        = 10^coef(BET_FDL_RD_LM_IO_FINAL)[1]*exp(var(residuals(BET_FDL_RD_LM_IO_FINAL))*2.651)
b_BET_FDL_RD_IO        = coef(BET_FDL_RD_LM_IO_FINAL)[2]

BET_FDL_RD_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"), SpeciesCode = "BET", SpeciesName = "Bigeye tuna", SpeciesScientific = c("Thunnus obesus"), Sex = "Mixed", Source = "LF", Target = "RD", SampleSize = c(TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "AO", .N], TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "IO", .N]), FirstDorsalLengthMin = c(TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "IO", min(first_dorsal_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "IO", min(first_dorsal_length, na.rm = TRUE)]), FirstDorsalLengthMax = c(TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "AO", max(first_dorsal_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "IO", max(first_dorsal_length, na.rm = TRUE)]), a = c(a_BET_FDL_RD_AO, a_BET_FDL_RD_IO), b = c(b_BET_FDL_RD_AO, b_BET_FDL_RD_IO), Reference = "IOTC", Origin = "THIS STUDY")

## SKIPJACK TUNA ####

# Geo-referenced data set
SKJ_SAMPLES = TUNA_SAMPLES[species_code_fao == "SKJ" & ProvCode %in% c("MONS", "ETRA", "EAFR", "WTRA", "GUIN", "CNRY")]

# Full model with covariates
SKJ_FDL_RD_LM_FULL = lm(log10RW ~ log10FDL + Province + Quarter + SchoolType + Sex, data = SKJ_SAMPLES[sex %in% c("M", "F") & aggregation %in% c("DFAD", "FSC")])

#stepAIC(SKJ_FDL_RD_LM_FULL)
# all covariates kept in the model

# Final model
SKJ_FDL_RD_LM_WITH_COV_FINAL = SKJ_FDL_RD_LM_FULL

SKJ_FDL_RD_LM_WITH_COV_FINAL_ANOVA = anova_table("Skipjack tuna", SKJ_FDL_RD_LM_WITH_COV_FINAL)
SKJ_FDL_RD_LM_WITH_COV_FINAL_R2    = (summary(SKJ_FDL_RD_LM_WITH_COV_FINAL))$r.squared

# Model without covariate
SKJ_FDL_RD_LM_AO_FINAL = lm(log10RW ~ log10FDL, data = TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "AO"])
SKJ_FDL_RD_LM_IO_FINAL = lm(log10RW ~ log10FDL, data = TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "IO"])

ANOVA_SKJ_FDL_RD_AO = anova_table("Skipjack tuna", SKJ_FDL_RD_LM_AO_FINAL)
R2_SKJ_FDL_RD_AO    = (summary(SKJ_FDL_RD_LM_AO_FINAL))$r.squared
a_SKJ_FDL_RD_AO     = 10^coef(SKJ_FDL_RD_LM_AO_FINAL)[1]*exp(var(residuals(SKJ_FDL_RD_LM_AO_FINAL))*2.651)
b_SKJ_FDL_RD_AO     = coef(SKJ_FDL_RD_LM_AO_FINAL)[2]

ANOVA_SKJ_FDL_RD_IO = anova_table("Skipjack tuna", SKJ_FDL_RD_LM_IO_FINAL)
R2_SKJ_FDL_RD_IO    = (summary(SKJ_FDL_RD_LM_IO_FINAL))$r.squared
a_SKJ_FDL_RD_IO     = 10^coef(SKJ_FDL_RD_LM_IO_FINAL)[1]*exp(var(residuals(SKJ_FDL_RD_LM_IO_FINAL))*2.651)
b_SKJ_FDL_RD_IO     = coef(SKJ_FDL_RD_LM_IO_FINAL)[2]

SKJ_FDL_RD_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"), SpeciesCode = "SKJ", SpeciesName = "Skipjack tuna", SpeciesScientific = c("Katsuwonus pelamis"), Sex = "Mixed", Source = "LF", Target = "RD", SampleSize = c(TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "AO", .N], TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "IO", .N]), FirstDorsalLengthMin = c(TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "IO", min(first_dorsal_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "IO", min(first_dorsal_length, na.rm = TRUE)]), FirstDorsalLengthMax = c(TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "AO", max(first_dorsal_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "IO", max(first_dorsal_length, na.rm = TRUE)]), a = c(a_SKJ_FDL_RD_AO, a_SKJ_FDL_RD_IO), b = c(b_SKJ_FDL_RD_AO, b_SKJ_FDL_RD_IO), Reference = "IOTC", Origin = "THIS STUDY")

# YELLOWFIN TUNA ####

# Geo-referenced data set
YFT_SAMPLES = TUNA_SAMPLES[species_code_fao == "YFT" & ProvCode %in% c("MONS", "ETRA", "EAFR", "GUIN", "ARAB", "WTRA")]

# Full model with covariates
YFT_FDL_RD_LM_FULL = lm(log10RW ~ log10FDL + Province + Quarter + SchoolType + Sex, data = YFT_SAMPLES[sex %in% c("M", "F") & aggregation %in% c("DFAD", "FSC")])

#stepAIC(YFT_FDL_RD_LM_FULL)
# all covariates kept in the model

# Final model
YFT_FDL_RD_LM_WITH_COV_FINAL = YFT_FDL_RD_LM_FULL

YFT_FDL_RD_LM_WITH_COV_FINAL_ANOVA = anova_table("Yellowfin tuna", YFT_FDL_RD_LM_WITH_COV_FINAL)
YFT_FDL_RD_LM_WITH_COV_FINAL_R2    = (summary(YFT_FDL_RD_LM_WITH_COV_FINAL))$r.squared

# Model without covariate
YFT_FDL_RD_LM_AO_FINAL = lm(log10RW ~ log10FDL, data = TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "AO"])
YFT_FDL_RD_LM_IO_FINAL = lm(log10RW ~ log10FDL, data = TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "IO"])

ANOVA_YFT_FDL_RD_AO = anova_table("Yellowfin tuna", YFT_FDL_RD_LM_AO_FINAL)
R2_YFT_FDL_RD_AO    = (summary(YFT_FDL_RD_LM_AO_FINAL))$r.squared
a_YFT_FDL_RD_AO     = 10^coef(YFT_FDL_RD_LM_AO_FINAL)[1]*exp(var(residuals(YFT_FDL_RD_LM_AO_FINAL))*2.651)
b_YFT_FDL_RD_AO     = coef(YFT_FDL_RD_LM_AO_FINAL)[2]

ANOVA_YFT_FDL_RD_IO = anova_table("Yellowfin tuna", YFT_FDL_RD_LM_IO_FINAL)
R2_YFT_FDL_RD_IO    = (summary(YFT_FDL_RD_LM_IO_FINAL))$r.squared
a_YFT_FDL_RD_IO     = 10^coef(YFT_FDL_RD_LM_IO_FINAL)[1]*exp(var(residuals(YFT_FDL_RD_LM_IO_FINAL))*2.651)
b_YFT_FDL_RD_IO     = coef(YFT_FDL_RD_LM_IO_FINAL)[2]

YFT_FDL_RD_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"), SpeciesCode = "YFT", SpeciesName = "Yellowfin tuna", SpeciesScientific = c("Katsuwonus pelamis"), Sex = "Mixed", Source = "LF", Target = "RD", SampleSize = c(TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "AO", .N], TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "IO", .N]), FirstDorsalLengthMin = c(TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "IO", min(first_dorsal_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "IO", min(first_dorsal_length, na.rm = TRUE)]), FirstDorsalLengthMax = c(TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "AO", max(first_dorsal_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "IO", max(first_dorsal_length, na.rm = TRUE)]), a = c(a_YFT_FDL_RD_AO, a_YFT_FDL_RD_IO), b = c(b_YFT_FDL_RD_AO, b_YFT_FDL_RD_IO), Reference = "IOTC", Origin = "THIS STUDY")

# PARAMATERS ESTIMATED IN THE STUDY ####

FDL_RD_PARAMS_ESTIMATED = rbindlist(list(BET_FDL_RD_PARAMS, SKJ_FDL_RD_PARAMS, YFT_FDL_RD_PARAMS))

# Build complete table of ANOVA results

FDL_RD_LM_WITH_COV_FINAL_ANOVAS = rbindlist(list(BET_FDL_RD_LM_WITH_COV_FINAL_ANOVA,
                                                SKJ_FDL_RD_LM_WITH_COV_FINAL_ANOVA,
                                                YFT_FDL_RD_LM_WITH_COV_FINAL_ANOVA))

FDL_RD_LM_WITH_COV_FINAL_ANOVAS_FT =
  FDL_RD_LM_WITH_COV_FINAL_ANOVAS %>% 
  flextable() %>%
  merge_at(i = 1:6, j = "Species", part = "body") %>%
  merge_at(i = 7:12, j = "Species", part = "body") %>%
  merge_at(i = 13:18, j = "Species", part = "body") %>%
  hline(i = c(6, 12), border = fp_border(width = 1)) %>%
  align(part = "header", j = c(2, 4:7), align = "center") %>%
  width(width = c(1.2, 1.1, 0.6, 1, 1, 1, 0.7)) %>%
  fix_border_issues()
