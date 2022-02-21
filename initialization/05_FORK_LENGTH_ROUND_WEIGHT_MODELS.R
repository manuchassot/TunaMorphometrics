
# FORK LENGTH - ROUND WEIGHT ####

## BIGEYE TUNA ####

# Geo-referenced data set 

BET_SAMPLES_FL_RD_FULL = TUNA_SAMPLES[species_code_fao == "BET" & ProvCode %in% c("MONS", "ETRA", "GUIN", "WTRA") & sex %in% c("M", "F") & SchoolType %in% c("DFAD", "FSC") & !is.na(fork_length) & !is.na(whole_weight_kg)]

# Full model with covariates

BET_FL_RD_LM_FULL = lm(log10RW ~ log10FL + Province  + Quarter + SchoolType + Sex, data = BET_SAMPLES_FL_RD_FULL)

# stepAIC(BET_FL_RD_LM_FULL)
# covariates SchoolType and Sex are not kept in the model

# Final model
BET_SAMPLES_FL_RD_COVARIATES = TUNA_SAMPLES[species_code_fao == "BET" & ProvCode %in% c("MONS", "ETRA", "GUIN", "WTRA") & !is.na(fork_length) & !is.na(whole_weight_kg)]

BET_FL_RD_LM_WITH_COV_FINAL = lm(log10RW ~ log10FL + Province + Quarter, data = BET_SAMPLES_FL_RD_COVARIATES)

BET_FL_RD_LM_WITH_COV_FINAL_ANOVA = anova_table(SpeciesName = "Bigeye tuna", LinearModel = BET_FL_RD_LM_WITH_COV_FINAL, MeanSquaresDigits = 3)
BET_FL_RD_LM_WITH_COV_FINAL_R2    = (summary(BET_FL_RD_LM_WITH_COV_FINAL))$r.squared

# Model without covariate

BET_SAMPLES_FL_RD = TUNA_SAMPLES[species_code_fao == "BET" & !is.na(fork_length) & !is.na(whole_weight_kg)]

BET_FL_RD_LM_AO_FINAL = lm(log10RW ~ log10FL, data = BET_SAMPLES_FL_RD[ocean_code == "AO"])
BET_FL_RD_LM_IO_FINAL = lm(log10RW ~ log10FL, data = BET_SAMPLES_FL_RD[ocean_code == "IO"])

BET_FL_RD_LM_AO_ANOVA = anova_table("Bigeye tuna", BET_FL_RD_LM_AO_FINAL)
BET_FL_RD_LM_AO_R2    = (summary(BET_FL_RD_LM_AO_FINAL))$r.squared
a_BET_FL_RD_AO        = 10^coef(BET_FL_RD_LM_AO_FINAL)[1]*exp(var(residuals(BET_FL_RD_LM_AO_FINAL))*2.651)
b_BET_FL_RD_AO         = coef(BET_FL_RD_LM_AO_FINAL)[2]

BET_FL_RD_LM_IO_ANOVA = anova_table("Bigeye tuna", BET_FL_RD_LM_IO_FINAL)
BET_FL_RD_LM_IO_R2    = (summary(BET_FL_RD_LM_IO_FINAL))$r.squared
a_BET_FL_RD_IO        = 10^coef(BET_FL_RD_LM_IO_FINAL)[1]*exp(var(residuals(BET_FL_RD_LM_IO_FINAL))*2.651)
b_BET_FL_RD_IO        = coef(BET_FL_RD_LM_IO_FINAL)[2]

BET_FL_RD_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"),
                              SpeciesCode = "BET",
                              SpeciesName = "Bigeye tuna",
                              SpeciesScientific = c("Thunnus obesus"),
                              Sex = "Mixed",
                              Source = "LF",
                              Target = "RD",
                              SampleSize = c(BET_SAMPLES_FL_RD[ocean_code == "AO", .N], BET_SAMPLES_FL_RD[ocean_code == "IO", .N]),
                              ForkLengthRange = c(BET_SAMPLES_FL_RD[ocean_code == "AO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")], BET_SAMPLES_FL_RD[ocean_code == "IO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")]),
                              RoundWeightRange = c(BET_SAMPLES_FL_RD[ocean_code == "AO", paste(sprintf("%.1f", range(whole_weight_kg)), collapse = "-")], BET_SAMPLES_FL_RD[ocean_code == "IO", paste(sprintf("%.1f", range(whole_weight_kg)), collapse = "-")]),
                              a = c(a_BET_FL_RD_AO, a_BET_FL_RD_IO),
                              b = c(b_BET_FL_RD_AO, b_BET_FL_RD_IO),
                              Reference = "IOTC",
                              Origin = "THIS STUDY")

## SKIPJACK TUNA ####

# Geo-referenced data set
SKJ_SAMPLES_FL_RD_FULL = TUNA_SAMPLES[species_code_fao == "SKJ" & ProvCode %in% c("MONS", "ETRA", "EAFR", "WTRA", "GUIN", "CNRY") & sex %in% c("M", "F") & SchoolType %in% c("DFAD", "FSC") & !is.na(fork_length) & !is.na(whole_weight_kg)]

# Full model with covariates
SKJ_FL_RD_LM_FULL = lm(log10RW ~ log10FL + Province + Quarter + SchoolType + Sex, data = SKJ_SAMPLES_FL_RD_FULL)

#stepAIC(SKJ_FL_RD_LM_FULL)
# sex removed from in the model

# Final model

SKJ_SAMPLES_FL_RD_COVARIATES = TUNA_SAMPLES[species_code_fao == "SKJ" & ProvCode %in% c("MONS", "ETRA", "EAFR", "WTRA", "GUIN", "CNRY") & SchoolType %in% c("DFAD", "FSC") & !is.na(fork_length) & !is.na(whole_weight_kg)]

SKJ_FL_RD_LM_WITH_COV_FINAL = lm(log10RW ~ log10FL + Province + Quarter + SchoolType + Sex, data = SKJ_SAMPLES_FL_RD_COVARIATES)

SKJ_FL_RD_LM_WITH_COV_FINAL_ANOVA = anova_table("Skipjack tuna", SKJ_FL_RD_LM_WITH_COV_FINAL)
SKJ_FL_RD_LM_WITH_COV_FINAL_R2    = (summary(SKJ_FL_RD_LM_WITH_COV_FINAL))$r.squared

# Model without covariate

SKJ_SAMPLES_FL_RD = TUNA_SAMPLES[species_code_fao == "SKJ" & !is.na(fork_length) & !is.na(whole_weight_kg)]

SKJ_FL_RD_LM_AO_FINAL = lm(log10RW ~ log10FL, data = SKJ_SAMPLES_FL_RD[ocean_code == "AO"])
SKJ_FL_RD_LM_IO_FINAL = lm(log10RW ~ log10FL, data = SKJ_SAMPLES_FL_RD[ocean_code == "IO"])

ANOVA_SKJ_FL_RD_AO = anova_table("Skipjack tuna", SKJ_FL_RD_LM_AO_FINAL, 1)
R2_SKJ_FL_RD_AO    = (summary(SKJ_FL_RD_LM_AO_FINAL))$r.squared
a_SKJ_FL_RD_AO     = 10^coef(SKJ_FL_RD_LM_AO_FINAL)[1]*exp(var(residuals(SKJ_FL_RD_LM_AO_FINAL))*2.651)
b_SKJ_FL_RD_AO     = coef(SKJ_FL_RD_LM_AO_FINAL)[2]

ANOVA_SKJ_FL_RD_IO = anova_table("Skipjack tuna", SKJ_FL_RD_LM_IO_FINAL, 1)
R2_SKJ_FL_RD_IO    = (summary(SKJ_FL_RD_LM_IO_FINAL))$r.squared
a_SKJ_FL_RD_IO     = 10^coef(SKJ_FL_RD_LM_IO_FINAL)[1]*exp(var(residuals(SKJ_FL_RD_LM_IO_FINAL))*2.651)
b_SKJ_FL_RD_IO     = coef(SKJ_FL_RD_LM_IO_FINAL)[2]

SKJ_FL_RD_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"),
                              SpeciesCode = "SKJ",
                              SpeciesName = "Skipjack tuna",
                              SpeciesScientific = c("Katsuwonus pelamis"),
                              Sex = "Mixed",
                              Source = "LF",
                              Target = "RD",
                              SampleSize = c(SKJ_SAMPLES_FL_RD[ocean_code == "AO", .N], SKJ_SAMPLES_FL_RD[ocean_code == "IO", .N]),
                              ForkLengthRange = c(SKJ_SAMPLES_FL_RD[ocean_code == "AO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")], SKJ_SAMPLES_FL_RD[ocean_code == "IO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")]),
                              RoundWeightRange = c(SKJ_SAMPLES_FL_RD[ocean_code == "AO", paste(sprintf("%.1f", range(whole_weight_kg)), collapse = "-")], SKJ_SAMPLES_FL_RD[ocean_code == "IO", paste(sprintf("%.1f", range(whole_weight_kg)), collapse = "-")]),
                              a = c(a_SKJ_FL_RD_AO, a_SKJ_FL_RD_IO),
                              b = c(b_SKJ_FL_RD_AO, b_SKJ_FL_RD_IO),
                              Reference = "IOTC",
                              Origin = "THIS STUDY")

# YELLOWFIN TUNA ####

# Geo-referenced data set
YFT_SAMPLES_FL_RD_COVARIATES = TUNA_SAMPLES[species_code_fao == "YFT" & ProvCode %in% c("MONS", "ETRA", "EAFR", "GUIN", "ARAB", "WTRA") & sex %in% c("M", "F") & SchoolType %in% c("DFAD", "FSC") & !is.na(fork_length) & !is.na(whole_weight_kg)]

# Full model with covariates
YFT_FL_RD_LM_FULL = lm(log10RW ~ log10FL + Province + Quarter + SchoolType + Sex, data = YFT_SAMPLES_FL_RD_COVARIATES)

#stepAIC(YFT_FL_RD_LM_FULL)
# all covariates kept in the model

# Final model
YFT_FL_RD_LM_WITH_COV_FINAL = YFT_FL_RD_LM_FULL

YFT_FL_RD_LM_WITH_COV_FINAL_ANOVA = anova_table("Yellowfin tuna", YFT_FL_RD_LM_WITH_COV_FINAL, 3)
YFT_FL_RD_LM_WITH_COV_FINAL_R2    = (summary(YFT_FL_RD_LM_WITH_COV_FINAL))$r.squared

# Model without covariate

YFT_SAMPLES_FL_RD = TUNA_SAMPLES[species_code_fao == "YFT" & !is.na(fork_length) & !is.na(whole_weight_kg)]

YFT_FL_RD_LM_AO_FINAL = lm(log10RW ~ log10FL, data = YFT_SAMPLES_FL_RD[ocean_code == "AO"])
YFT_FL_RD_LM_IO_FINAL = lm(log10RW ~ log10FL, data = YFT_SAMPLES_FL_RD[ocean_code == "IO"])

ANOVA_YFT_FL_RD_AO = anova_table("Yellowfin tuna", YFT_FL_RD_LM_AO_FINAL, 3)
R2_YFT_FL_RD_AO    = (summary(YFT_FL_RD_LM_AO_FINAL))$r.squared
a_YFT_FL_RD_AO     = 10^coef(YFT_FL_RD_LM_AO_FINAL)[1]*exp(var(residuals(YFT_FL_RD_LM_AO_FINAL))*2.651)
b_YFT_FL_RD_AO     = coef(YFT_FL_RD_LM_AO_FINAL)[2]

ANOVA_YFT_FL_RD_IO = anova_table("Yellowfin tuna", YFT_FL_RD_LM_IO_FINAL, 3)
R2_YFT_FL_RD_IO    = (summary(YFT_FL_RD_LM_IO_FINAL))$r.squared
a_YFT_FL_RD_IO     = 10^coef(YFT_FL_RD_LM_IO_FINAL)[1]*exp(var(residuals(YFT_FL_RD_LM_IO_FINAL))*2.651)
b_YFT_FL_RD_IO     = coef(YFT_FL_RD_LM_IO_FINAL)[2]

YFT_FL_RD_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"),
                              SpeciesCode = "YFT",
                              SpeciesName = "Yellowfin tuna",
                              SpeciesScientific = c("Thunnus albacares"),
                              Sex = "Mixed",
                              Source = "LF",
                              Target = "RD",
                              SampleSize = c(YFT_SAMPLES_FL_RD[ocean_code == "AO", .N], YFT_SAMPLES_FL_RD[ocean_code == "IO", .N]),
                              ForkLengthRange = c(YFT_SAMPLES_FL_RD[ocean_code == "AO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")], YFT_SAMPLES_FL_RD[ocean_code == "IO", paste(sprintf("%.1f", range(fork_length)), collapse = "-")]),
                              RoundWeightRange = c(YFT_SAMPLES_FL_RD[ocean_code == "AO", paste(sprintf("%.1f", range(whole_weight_kg)), collapse = "-")], YFT_SAMPLES_FL_RD[ocean_code == "IO", paste(sprintf("%.1f", range(whole_weight_kg)), collapse = "-")]),
                              a = c(a_YFT_FL_RD_AO, a_YFT_FL_RD_IO),
                              b = c(b_YFT_FL_RD_AO, b_YFT_FL_RD_IO),
                              Reference = "IOTC",
                              Origin = "THIS STUDY")

# PARAMATERS ESTIMATED IN THE STUDY ####

FL_RD_PARAMS_ESTIMATED = rbindlist(list(BET_FL_RD_PARAMS, SKJ_FL_RD_PARAMS, YFT_FL_RD_PARAMS))

# Build complete table of ANOVA results

FL_RD_LM_WITH_COV_FINAL_ANOVAS = rbindlist(list(BET_FL_RD_LM_WITH_COV_FINAL_ANOVA,
                                                SKJ_FL_RD_LM_WITH_COV_FINAL_ANOVA,
                                                YFT_FL_RD_LM_WITH_COV_FINAL_ANOVA))

FL_RD_LM_WITH_COV_FINAL_ANOVAS_FT =
  FL_RD_LM_WITH_COV_FINAL_ANOVAS %>% 
  flextable() %>%
  merge_at(i = 1:4, j = "Species", part = "body") %>%
  merge_at(i = 5:10, j = "Species", part = "body") %>%
  merge_at(i = 11:16, j = "Species", part = "body") %>%
  hline(i = c(4, 10), border = fp_border(width = 1)) %>%
  align(part = "header", j = c(2, 4:7), align = "center") %>%
  width(width = c(1.2, 1.1, 0.6, 1, 1, 1, 0.7)) %>%
  fix_border_issues()
