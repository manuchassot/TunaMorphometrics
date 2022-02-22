
# TEMP: ERRORS TO CHECK

# Errors in weight
ERRORS_RD = TUNA_SAMPLES[organism_identifier %in% c("SKJ_20210812_21", "SKJ_20210727_17", "IOT_2016_0405", "IOT_2016_0422", "IOT_2974", "IOT_2454", "IOT2014-016", "IOT_0247", "IOT_3363", "IOT_3366", "IOT_3672", "IOT_3755", "SKJ-09.M1.L17.7", "SKJ-09.M1.L25.23", "SKJ-09.M2.L10.16", "IOT_2938", "SA01893", "DCF-AO-1280", "SKJ-09.M3.L22.4", "IOT_2016_0414", "IOT_3460", "SKJ_20210916_17", "SKJ_20210916_39", "IOT_3128", "IOT_0588", "IOT_3685", "SKJ_20210824_30", "SKJ_20210824_4", "SKJ_20210907_19", "SKJ_20210826_1", "IOT_2016_0416", "IOT_2016_0423", "IOT_2016_0431", "IOT_1033", "SKJ_20210916_31", "IOT_0613", "SKJ_20210701_1", "IOT_2016_0442", "IOT_2494", "IOT_2508", "IOT_2016_0420", "IOT_2016_0458", "IOT_2016_0488", "IOT_0481", "IOT_3365", "IOT_2490", "SKJ_20210701_14", "AA1958", "DCF-AO-1107", "DCF-AO-1372", "DCF6878", "IOT24", "OT11610", "A260", "YFT-09.M2.L5.4","IOT_2016_0305", "YFT-09.M2.L4.12", "DCF0031", "DCF18985", "DCF_18985"), .(ocean_code, project, organism_sampling_date, organism_identifier_origin, organism_identifier, species_code_fao, first_dorsal_length, fork_length, whole_weight_kg, gutted_weight_kg)]
                                    
ERRORS_RD[, correction := "whole_weight_kg set to NA"]                              

# Errors in fork length
ERRORS_FL = TUNA_SAMPLES[organism_identifier %in% c("SA0031", "SA01665", "SKJ_20210503_11", "SA0904", "SA0624", "SA0782", "SA0773", "SA0777", "SA01132", "SA01034", "IOT_5065", "IOT_5066", "IOT_5067", "IOT_5011", "IOT_4675", "IOT_4871", "IOT_4585"), .(ocean_code, project, organism_sampling_date, organism_identifier_origin, organism_identifier, species_code_fao, first_dorsal_length, fork_length, whole_weight_kg, gutted_weight_kg)]
ERRORS_FL[, correction := "fork_length set to NA"]

# Errors in first dorsal length
ERRORS_FDL1 = TUNA_SAMPLES[organism_identifier == "DCF-AO-25", .(ocean_code, project, organism_sampling_date, organism_identifier_origin, organism_identifier, species_code_fao, first_dorsal_length, fork_length, whole_weight_kg, gutted_weight_kg)][order(ocean_code, species_code_fao, organism_sampling_date)]

ERRORS_FDL1[, correction := "first_dorsal_length set to 35.5 instead of 25.5 assuming round weight is correct (?)"]

ERRORS_FDL2 = TUNA_SAMPLES[organism_identifier == "DCF-AO-414", .(ocean_code, project, organism_sampling_date, organism_identifier_origin, organism_identifier, species_code_fao, first_dorsal_length, fork_length, whole_weight_kg, gutted_weight_kg)][order(ocean_code, species_code_fao, organism_sampling_date)]

ERRORS_FDL2[, correction := "first_dorsal_length set to 41 instead of 31 assuming round weight is correct (?)"]

ERRORS_FDL3 = TUNA_SAMPLES[organism_identifier == "SKJ_20210701_14", .(ocean_code, project, organism_sampling_date, organism_identifier_origin, organism_identifier, species_code_fao, first_dorsal_length, fork_length, whole_weight_kg, gutted_weight_kg)][order(ocean_code, species_code_fao, organism_sampling_date)]

ERRORS_FDL3[, correction := "first_dorsal_length set to 19 instead of 29 assuming round weight is correct (?)"]

# Combine all errors

ERRORS_MORPHO = rbindlist(list(ERRORS_RD, ERRORS_FL, ERRORS_FDL1, ERRORS_FDL2, ERRORS_FDL3))[order(ocean_code, species_code_fao, organism_sampling_date)]

write.xlsx(ERRORS_MORPHO, file = "../inputs/data/OBVIOUS_ERRORS_MORPHOMETRIC_VALUES.xlsx")

