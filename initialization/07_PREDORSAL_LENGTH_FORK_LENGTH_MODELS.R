
# PREDORSAL LENGTH - FORK LENGTH ####

## BIGEYE TUNA ####

# Geo-referenced data set
BET_SAMPLES_FDL_FL_COVARIATES = TUNA_SAMPLES[species_code_fao == "BET" & ProvCode %in% c("MONS", "ETRA", "GUIN", "WTRA") & sex %in% c("M", "F") & aggregation %in% c("DFAD", "FSC") & !is.na(first_dorsal_length) & !is.na(fork_length)]

# Full model with covariates
BET_FDL_FL_LM_FULL = lm(fork_length ~ first_dorsal_length + Province  + Quarter + SchoolType + Sex, data = BET_SAMPLES_FDL_FL_COVARIATES)

# stepAIC(BET_FDL_FL_LM_FULL)

# Final model
BET_FDL_FL_LM_WITH_COV_FINAL = BET_FDL_FL_LM_FULL

BET_FDL_FL_LM_WITH_COV_FINAL_ANOVA = anova_table(SpeciesName = "Bigeye tuna", LinearModel = BET_FDL_FL_LM_WITH_COV_FINAL, MeanSquaresDigits = 1)
BET_FDL_FL_LM_WITH_COV_FINAL_R2    = (summary(BET_FDL_FL_LM_WITH_COV_FINAL))$r.squared

# Model without covariate

BET_SAMPLES_FDL_FL = TUNA_SAMPLES[species_code_fao == "BET" & !is.na(first_dorsal_length) & !is.na(fork_length)]

BET_FDL_FL_LM_AO_FINAL = lm(fork_length ~ first_dorsal_length, data = BET_SAMPLES_FDL_FL[ocean_code == "AO"])
BET_FDL_FL_LM_IO_FINAL = lm(fork_length ~ first_dorsal_length, data = BET_SAMPLES_FDL_FL[ocean_code == "IO"])

BET_FDL_FL_LM_AO_ANOVA = anova_table("Bigeye tuna", BET_FDL_FL_LM_AO_FINAL)
BET_FDL_FL_LM_AO_R2    = (summary(BET_FDL_FL_LM_AO_FINAL))$r.squared
a_BET_FDL_FL_AO        = coef(BET_FDL_FL_LM_AO_FINAL)[1]
b_BET_FDL_FL_AO        = coef(BET_FDL_FL_LM_AO_FINAL)[2]

BET_FDL_FL_LM_IO_ANOVA = anova_table("Bigeye tuna", BET_FDL_FL_LM_IO_FINAL)
BET_FDL_FL_LM_IO_R2    = (summary(BET_FDL_FL_LM_IO_FINAL))$r.squared
a_BET_FDL_FL_IO        = coef(BET_FDL_FL_LM_IO_FINAL)[1]
b_BET_FDL_FL_IO        = coef(BET_FDL_FL_LM_IO_FINAL)[2]

BET_FDL_FL_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"),
                               SpeciesCode = "BET",
                               SpeciesName = "Bigeye tuna",
                               SpeciesScientific = c("Thunnus obesus"),
                               Sex = "Mixed",
                               Source = "LD",
                               Target = "LF",
                               SampleSize = c(BET_SAMPLES_FDL_FL[ocean_code == "AO", .N], BET_SAMPLES_FDL_FL[ocean_code == "IO", .N]),
                               FirstDorsalLengthRange = c(BET_SAMPLES_FDL_FL[ocean_code == "AO", paste(sprintf("%.1f", range(first_dorsal_length)), collapse = "-")], BET_SAMPLES_FDL_FL[ocean_code == "IO", paste(sprintf("%.1f", range(first_dorsal_length)), collapse = "-")]),
                               ForkLengthRange = c(BET_SAMPLES_FDL_FL[ocean_code == "AO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")], BET_SAMPLES_FDL_FL[ocean_code == "IO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")]),
                               a = c(a_BET_FDL_FL_AO, a_BET_FDL_FL_IO),
                               b = c(b_BET_FDL_FL_AO, b_BET_FDL_FL_IO),
                               Reference = "IOTC",
                               Origin = "THIS STUDY")

## SKIPJACK TUNA ####

# Geo-referenced data set
SKJ_FDL_FL_SAMPLES_COVARIATES = TUNA_SAMPLES[species_code_fao == "SKJ" & ProvCode %in% c("MONS", "ETRA", "EAFR", "WTRA", "GUIN", "CNRY") & sex %in% c("M", "F") & aggregation %in% c("DFAD", "FSC") & !is.na(first_dorsal_length) & !is.na(fork_length)]

# Full model with covariates
SKJ_FDL_FL_LM_FULL = lm(first_dorsal_length ~ fork_length + Province + Quarter + SchoolType + Sex, data = SKJ_FDL_FL_SAMPLES_COVARIATES)

# stepAIC(SKJ_FDL_FL_LM_FULL)
# all covariates kept in the model

# Final model
SKJ_FDL_FL_LM_WITH_COV_FINAL = SKJ_FDL_FL_LM_FULL

SKJ_FDL_FL_LM_WITH_COV_FINAL_ANOVA = anova_table(SpeciesName = "Skipjack tuna", LinearModel = SKJ_FDL_FL_LM_WITH_COV_FINAL, MeanSquaresDigits = 1)
SKJ_FDL_FL_LM_WITH_COV_FINAL_R2    = (summary(SKJ_FDL_FL_LM_WITH_COV_FINAL))$r.squared

# Model without covariate

SKJ_SAMPLES_FDL_FL = TUNA_SAMPLES[species_code_fao == "SKJ" & !is.na(first_dorsal_length) & !is.na(fork_length)]

SKJ_FDL_FL_LM_AO_FINAL = lm(fork_length ~ first_dorsal_length, data = SKJ_SAMPLES_FDL_FL[ocean_code == "AO"])
SKJ_FDL_FL_LM_IO_FINAL = lm(fork_length ~ first_dorsal_length, data = SKJ_SAMPLES_FDL_FL[ocean_code == "IO"])

ANOVA_SKJ_FDL_FL_AO = anova_table("Skipjack tuna", SKJ_FDL_FL_LM_AO_FINAL)
R2_SKJ_FDL_FL_AO    = (summary(SKJ_FDL_FL_LM_AO_FINAL))$r.squared
a_SKJ_FDL_FL_AO     = coef(SKJ_FDL_FL_LM_AO_FINAL)[1]
b_SKJ_FDL_FL_AO     = coef(SKJ_FDL_FL_LM_AO_FINAL)[2]

ANOVA_SKJ_FDL_FL_IO = anova_table("Skipjack tuna", SKJ_FDL_FL_LM_IO_FINAL)
R2_SKJ_FDL_FL_IO    = (summary(SKJ_FDL_FL_LM_IO_FINAL))$r.squared
a_SKJ_FDL_FL_IO     = coef(SKJ_FDL_FL_LM_IO_FINAL)[1]
b_SKJ_FDL_FL_IO     = coef(SKJ_FDL_FL_LM_IO_FINAL)[2]

SKJ_FDL_FL_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"),
                               SpeciesCode = "SKJ",
                               SpeciesName = "Skipjack tuna",
                               SpeciesScientific = c("Katsuwonus pelamis"),
                               Sex = "Mixed",
                               Source = "LD",
                               Target = "LF",
                               SampleSize = c(SKJ_SAMPLES_FDL_FL[ocean_code == "AO", .N], SKJ_SAMPLES_FDL_FL[ocean_code == "IO", .N]),
                               FirstDorsalLengthRange = c(SKJ_SAMPLES_FDL_FL[ocean_code == "AO", paste(sprintf("%.1f", range(first_dorsal_length)), collapse = "-")], SKJ_SAMPLES_FDL_FL[ocean_code == "IO", paste(sprintf("%.1f", range(first_dorsal_length)), collapse = "-")]),
                               ForkLengthRange = c(SKJ_SAMPLES_FDL_FL[ocean_code == "AO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")], SKJ_SAMPLES_FDL_FL[ocean_code == "IO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")]),
                               a = c(a_SKJ_FDL_FL_AO, a_SKJ_FDL_FL_IO),
                               b = c(b_SKJ_FDL_FL_AO, b_SKJ_FDL_FL_IO),
                               Reference = "IOTC",
                               Origin = "THIS STUDY")

## YELLOWFIN TUNA ####

# Geo-referenced data set
YFT_FDL_FL_SAMPLES_COVARIATES = TUNA_SAMPLES[species_code_fao == "YFT" & ProvCode %in% c("MONS", "ETRA", "EAFR", "GUIN", "ARAB", "WTRA") & sex %in% c("M", "F") & aggregation %in% c("DFAD", "FSC") & !is.na(first_dorsal_length) & !is.na(fork_length)]

# Full model with covariates
YFT_FDL_FL_LM_FULL = lm(fork_length ~ first_dorsal_length + Province + Quarter + SchoolType + Sex, data = YFT_FDL_FL_SAMPLES_COVARIATES)

# stepAIC(YFT_FDL_FL_LM_FULL)
# all covariates kept in the model

# Final model
YFT_FDL_FL_LM_WITH_COV_FINAL = YFT_FDL_FL_LM_FULL

YFT_FDL_FL_LM_WITH_COV_FINAL_ANOVA = anova_table(SpeciesName = "Yellowfin tuna", LinearModel = YFT_FDL_FL_LM_WITH_COV_FINAL, MeanSquaresDigits = 1)
YFT_FDL_FL_LM_WITH_COV_FINAL_R2    = (summary(YFT_FDL_FL_LM_WITH_COV_FINAL))$r.squared

# Model without covariate

YFT_SAMPLES_FDL_FL = TUNA_SAMPLES[species_code_fao == "YFT" & !is.na(first_dorsal_length) & !is.na(fork_length)]

YFT_FDL_FL_LM_AO_FINAL = lm(fork_length ~ first_dorsal_length, data = YFT_SAMPLES_FDL_FL[ocean_code == "AO"])
YFT_FDL_FL_LM_IO_FINAL = lm(fork_length ~ first_dorsal_length, data = YFT_SAMPLES_FDL_FL[ocean_code == "IO"])

ANOVA_YFT_FDL_FL_AO = anova_table("Yellowfin tuna", YFT_FDL_FL_LM_AO_FINAL)
R2_YFT_FDL_FL_AO    = (summary(YFT_FDL_FL_LM_AO_FINAL))$r.squared
a_YFT_FDL_FL_AO     = coef(YFT_FDL_FL_LM_AO_FINAL)[1]
b_YFT_FDL_FL_AO     = coef(YFT_FDL_FL_LM_AO_FINAL)[2]

ANOVA_YFT_FDL_FL_IO = anova_table("Yellowfin tuna", YFT_FDL_FL_LM_IO_FINAL)
R2_YFT_FDL_FL_IO    = (summary(YFT_FDL_FL_LM_IO_FINAL))$r.squared
a_YFT_FDL_FL_IO     = coef(YFT_FDL_FL_LM_IO_FINAL)[1]
b_YFT_FDL_FL_IO     = coef(YFT_FDL_FL_LM_IO_FINAL)[2]

YFT_FDL_FL_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"),
                               SpeciesCode = "YFT",
                               SpeciesName = "Yellowfin tuna",
                               SpeciesScientific = c("Thunnus albacares"),
                               Sex = "Mixed",
                               Source = "LD",
                               Target = "LF",
                               SampleSize = c(YFT_SAMPLES_FDL_FL[ocean_code == "AO", .N], YFT_SAMPLES_FDL_FL[ocean_code == "IO", .N]),
                               FirstDorsalLengthRange = c(YFT_SAMPLES_FDL_FL[ocean_code == "AO", paste(sprintf("%.1f", range(first_dorsal_length)), collapse = "-")], YFT_SAMPLES_FDL_FL[ocean_code == "IO", paste(sprintf("%.1f", range(first_dorsal_length)), collapse = "-")]),
                               ForkLengthRange = c(YFT_SAMPLES_FDL_FL[ocean_code == "AO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")], YFT_SAMPLES_FDL_FL[ocean_code == "IO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")]),
                               a = c(a_YFT_FDL_FL_AO, a_YFT_FDL_FL_IO),
                               b = c(b_YFT_FDL_FL_AO, b_YFT_FDL_FL_IO),
                               Reference = "IOTC",
                               Origin = "THIS STUDY")


# PARAMATERS ESTIMATED IN THE STUDY ####

FDL_FL_PARAMS_ESTIMATED = rbindlist(list(BET_FDL_FL_PARAMS, SKJ_FDL_FL_PARAMS, YFT_FDL_FL_PARAMS))

# Build complete table of ANOVA results

FDL_FL_LM_WITH_COV_FINAL_ANOVAS = rbindlist(list(BET_FDL_FL_LM_WITH_COV_FINAL_ANOVA,
                                                 SKJ_FDL_FL_LM_WITH_COV_FINAL_ANOVA,
                                                 YFT_FDL_FL_LM_WITH_COV_FINAL_ANOVA))

FDL_FL_LM_WITH_COV_FINAL_ANOVAS_FT =
  FDL_FL_LM_WITH_COV_FINAL_ANOVAS %>% 
  flextable() %>%
  merge_at(i = 1:6, j = "Species", part = "body") %>%
  merge_at(i = 7:12, j = "Species", part = "body") %>%
  merge_at(i = 13:18, j = "Species", part = "body") %>%
  hline(i = c(6, 12), border = fp_border(width = 1)) %>%
  align(part = "header", j = c(2, 4:7), align = "center") %>%
  width(width = c(1.2, 1.1, 0.6, 1, 1, 1, 0.7)) %>%
  fix_border_issues()
