
# FORK LENGTH - ROUND WEIGHT ####

# Prepare the data
TUNA_SAMPLES[, log10FL  := log(fork_length, 10)]
TUNA_SAMPLES[, log10RW  := log(whole_weight_kg, 10)]
TUNA_SAMPLES[, Quarter  := factor(capture_quarter)]
TUNA_SAMPLES[, Province := factor(ProvCode)]
TUNA_SAMPLES[, SchoolType := factor(aggregation)]
TUNA_SAMPLES[, Sex := factor(sex)]

## BIGEYE TUNA ####

# Geo-referenced data set 
BET_SAMPLES = TUNA_SAMPLES[species_code_fao == "BET" & ProvCode %in% c("MONS", "ETRA", "GUIN", "WTRA")]

# Full model with covariates
BET_FL_RD_LM_FULL = lm(log10RW ~ log10FL + Province  + Quarter + SchoolType + Sex, data = BET_SAMPLES[sex %in% c("M", "F") & aggregation %in% c("DFAD", "FSC")])

# stepAIC(BET_FL_RD_LM_FULL)
# covariates SchoolType and Sex are not kept in the model

# Final model
BET_FL_RD_LM_WITH_COV_FINAL = lm(log10RW ~ log10FL + Province + Quarter, data = BET_SAMPLES)

BET_FL_RD_LM_WITH_COV_FINAL_ANOVA = anova_table("Bigeye tuna", BET_FL_RD_LM_WITH_COV_FINAL)
BET_FL_RD_LM_WITH_COV_FINAL_R2    = (summary(BET_FL_RD_LM_WITH_COV_FINAL))$r.squared

# Model without covariate
BET_FL_RD_LM_AO_FINAL = lm(log10RW ~ log10FL, data = TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "AO"])
BET_FL_RD_LM_IO_FINAL = lm(log10RW ~ log10FL, data = TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "IO"])

BET_FL_RD_LM_AO_ANOVA = anova_table("Bigeye tuna", BET_FL_RD_LM_AO_FINAL)
BET_FL_RD_LM_AO_R2    = (summary(BET_FL_RD_LM_AO_FINAL))$r.squared
a_BET_FL_RD_AO        = 10^coef(BET_FL_RD_LM_AO_FINAL)[1]*exp(var(residuals(BET_FL_RD_LM_AO_FINAL))*2.651)
b_BET_FL_RD_AO         = coef(BET_FL_RD_LM_AO_FINAL)[2]

BET_FL_RD_LM_IO_ANOVA = anova_table("Bigeye tuna", BET_FL_RD_LM_IO_FINAL)
BET_FL_RD_LM_IO_R2    = (summary(BET_FL_RD_LM_IO_FINAL))$r.squared
a_BET_FL_RD_IO        = 10^coef(BET_FL_RD_LM_IO_FINAL)[1]*exp(var(residuals(BET_FL_RD_LM_IO_FINAL))*2.651)
b_BET_FL_RD_IO        = coef(BET_FL_RD_LM_IO_FINAL)[2]

BET_FL_RD_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"), SpeciesCode = "BET", SpeciesName = "Bigeye tuna", SpeciesScientific = c("Thunnus obesus"), Sex = "Mixed", Source = "LF", Target = "RD", SampleSize = c(TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "AO", .N], TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "IO", .N]), ForkLengthMin = c(TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "IO", min(fork_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "IO", min(fork_length, na.rm = TRUE)]), ForkLengthMax = c(TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "AO", max(fork_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "BET" & ocean_code == "IO", max(fork_length, na.rm = TRUE)]), a = c(a_BET_FL_RD_AO, a_BET_FL_RD_IO), b = c(b_BET_FL_RD_AO, b_BET_FL_RD_IO), Reference = "IOTC", Origin = "THIS STUDY")

## SKIPJACK TUNA ####

# Geo-referenced data set
SKJ_SAMPLES = TUNA_SAMPLES[species_code_fao == "SKJ" & ProvCode %in% c("MONS", "ETRA", "EAFR", "WTRA", "GUIN", "CNRY")]

# Full model with covariates
SKJ_FL_RD_LM_FULL = lm(log10RW ~ log10FL + Province + Quarter + SchoolType + Sex, data = SKJ_SAMPLES[sex %in% c("M", "F") & aggregation %in% c("DFAD", "FSC")])

#stepAIC(SKJ_FL_RD_LM_FULL)
# all covariates kept in the model

# Final model
SKJ_FL_RD_LM_WITH_COV_FINAL = SKJ_FL_RD_LM_FULL

SKJ_FL_RD_LM_WITH_COV_FINAL_ANOVA = anova_table("Skipjack tuna", SKJ_FL_RD_LM_WITH_COV_FINAL)
SKJ_FL_RD_LM_WITH_COV_FINAL_R2    = (summary(SKJ_FL_RD_LM_WITH_COV_FINAL))$r.squared

# Model without covariate
SKJ_FL_RD_LM_AO_FINAL = lm(log10RW ~ log10FL, data = TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "AO"])
SKJ_FL_RD_LM_IO_FINAL = lm(log10RW ~ log10FL, data = TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "IO"])

ANOVA_SKJ_FL_RD_AO = anova_table("Skipjack tuna", SKJ_FL_RD_LM_AO_FINAL)
R2_SKJ_FL_RD_AO    = (summary(SKJ_FL_RD_LM_AO_FINAL))$r.squared
a_SKJ_FL_RD_AO     = 10^coef(SKJ_FL_RD_LM_AO_FINAL)[1]*exp(var(residuals(SKJ_FL_RD_LM_AO_FINAL))*2.651)
b_SKJ_FL_RD_AO     = coef(SKJ_FL_RD_LM_AO_FINAL)[2]

ANOVA_SKJ_FL_RD_IO = anova_table("Skipjack tuna", SKJ_FL_RD_LM_IO_FINAL)
R2_SKJ_FL_RD_IO    = (summary(SKJ_FL_RD_LM_IO_FINAL))$r.squared
a_SKJ_FL_RD_IO     = 10^coef(SKJ_FL_RD_LM_IO_FINAL)[1]*exp(var(residuals(SKJ_FL_RD_LM_IO_FINAL))*2.651)
b_SKJ_FL_RD_IO     = coef(SKJ_FL_RD_LM_IO_FINAL)[2]

SKJ_FL_RD_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"), SpeciesCode = "SKJ", SpeciesName = "Skipjack tuna", SpeciesScientific = c("Katsuwonus pelamis"), Sex = "Mixed", Source = "LF", Target = "RD", SampleSize = c(TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "AO", .N], TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "IO", .N]), ForkLengthMin = c(TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "IO", min(fork_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "IO", min(fork_length, na.rm = TRUE)]), ForkLengthMax = c(TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "AO", max(fork_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "SKJ" & ocean_code == "IO", max(fork_length, na.rm = TRUE)]), a = c(a_SKJ_FL_RD_AO, a_SKJ_FL_RD_IO), b = c(b_SKJ_FL_RD_AO, b_SKJ_FL_RD_IO), Reference = "IOTC", Origin = "THIS STUDY")

# YELLOWFIN TUNA ####

# Geo-referenced data set
YFT_SAMPLES = TUNA_SAMPLES[species_code_fao == "YFT" & ProvCode %in% c("MONS", "ETRA", "EAFR", "GUIN", "ARAB", "WTRA")]

# Full model with covariates
YFT_FL_RD_LM_FULL = lm(log10RW ~ log10FL + Province + Quarter + SchoolType + Sex, data = YFT_SAMPLES[sex %in% c("M", "F") & aggregation %in% c("DFAD", "FSC")])

#stepAIC(YFT_FL_RD_LM_FULL)
# all covariates kept in the model

# Final model
YFT_FL_RD_LM_WITH_COV_FINAL = YFT_FL_RD_LM_FULL

YFT_FL_RD_LM_WITH_COV_FINAL_ANOVA = anova_table("Yellowfin tuna", YFT_FL_RD_LM_WITH_COV_FINAL)
YFT_FL_RD_LM_WITH_COV_FINAL_R2    = (summary(YFT_FL_RD_LM_WITH_COV_FINAL))$r.squared

# Model without covariate
YFT_FL_RD_LM_AO_FINAL = lm(log10RW ~ log10FL, data = TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "AO"])
YFT_FL_RD_LM_IO_FINAL = lm(log10RW ~ log10FL, data = TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "IO"])

ANOVA_YFT_FL_RD_AO = anova_table("Yellowfin tuna", YFT_FL_RD_LM_AO_FINAL)
R2_YFT_FL_RD_AO    = (summary(YFT_FL_RD_LM_AO_FINAL))$r.squared
a_YFT_FL_RD_AO     = 10^coef(YFT_FL_RD_LM_AO_FINAL)[1]*exp(var(residuals(YFT_FL_RD_LM_AO_FINAL))*2.651)
b_YFT_FL_RD_AO     = coef(YFT_FL_RD_LM_AO_FINAL)[2]

ANOVA_YFT_FL_RD_IO = anova_table("Yellowfin tuna", YFT_FL_RD_LM_IO_FINAL)
R2_YFT_FL_RD_IO    = (summary(YFT_FL_RD_LM_IO_FINAL))$r.squared
a_YFT_FL_RD_IO     = 10^coef(YFT_FL_RD_LM_IO_FINAL)[1]*exp(var(residuals(YFT_FL_RD_LM_IO_FINAL))*2.651)
b_YFT_FL_RD_IO     = coef(YFT_FL_RD_LM_IO_FINAL)[2]

YFT_FL_RD_PARAMS = data.table(Ocean = c("Atlantic Ocean", "Indian Ocean"), SpeciesCode = "YFT", SpeciesName = "Yellowfin tuna", SpeciesScientific = c("Katsuwonus pelamis"), Sex = "Mixed", Source = "LF", Target = "RD", SampleSize = c(TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "AO", .N], TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "IO", .N]), ForkLengthMin = c(TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "IO", min(fork_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "IO", min(fork_length, na.rm = TRUE)]), ForkLengthMax = c(TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "AO", max(fork_length, na.rm = TRUE)], TUNA_SAMPLES[species_code_fao == "YFT" & ocean_code == "IO", max(fork_length, na.rm = TRUE)]), a = c(a_YFT_FL_RD_AO, a_YFT_FL_RD_IO), b = c(b_YFT_FL_RD_AO, b_YFT_FL_RD_IO), Reference = "IOTC", Origin = "THIS STUDY")

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

# Combine review data set of parameters with new estimates

FL_RD_PARAMS = rbindlist(list(FL_RD_PARAMS_REVIEW, FL_RD_PARAMS_ESTIMATED))
FL_RD_PARAMS[Origin != "THIS STUDY" , CombinedSource := "Literature review"]
FL_RD_PARAMS[Origin == "THIS STUDY",  CombinedSource := "This Study"]

# Factorize and order the species
FL_RD_PARAMS[, SpeciesName := factor(SpeciesName, levels = c("Bigeye tuna", "Yellowfin tuna", "Skipjack tuna"), ordered = TRUE)]

FL_RD_PARAMS_TABLE_STUDY = FL_RD_PARAMS[CombinedSource != "Literature review", .(Ocean, SpeciesCode, SpeciesName, Sex, SampleSize, ForkLengthMin, ForkLengthMax, a, b)][order(Ocean, -SpeciesName)]

FL_RD_PARAMS_TABLE_STUDY_FT =
  FL_RD_PARAMS_TABLE_STUDY %>%
  flextable() %>%
  set_header_labels(SpeciesCode = "Species code", SpeciesName = "Species name", SampleSize = "Sample size") %>%
  align(j = c("a", "b"), part = "header", align = "center") %>%
  hline(i = 3) %>%
  autofit()



FL_RD_PARAMS_TABLE_REVIEW = FL_RD_PARAMS[CombinedSource == "Literature review", .(Ocean, SpeciesCode, SpeciesName, Sex, SampleSize, ForkLengthMin, ForkLengthMax, a, b, Reference)][order(SpeciesCode, Ocean)]


