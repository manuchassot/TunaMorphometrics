
print("Read morphometric data...")

# Data collected as per EU (DCF and EUMAP) ####

## Indian Ocean ####
IO_RAW_SAMPLES_WITH_ENVIRONMENT = fread("../inputs/data/AAD IND Chassot 2021.txt")

# Update ocean_code when missing and information shows the fish is from the Indian Ocean
IO_RAW_SAMPLES_WITH_ENVIRONMENT[is.na(ocean_code) & vessel_name == "PLAYA DE ANZORAS", ocean_code := "IO"]

# Convert NA character to NA
IO_RAW_SAMPLES_WITH_ENVIRONMENT[geometry == "NA", geometry := NA]

# No information on origin of the fish
IO_INDIC_NO_OCEAN_INFORMATION = is.na(IO_RAW_SAMPLES_WITH_ENVIRONMENT$ocean_code)
IO_RAW_SAMPLES_WITH_ENVIRONMENT1 = IO_RAW_SAMPLES_WITH_ENVIRONMENT[!IO_INDIC_NO_OCEAN_INFORMATION]

# Ocean code different than IO
IO_INDIC_OTHER_OCEAN = IO_RAW_SAMPLES_WITH_ENVIRONMENT1$ocean_code != "IO"
IO_RAW_SAMPLES_WITH_ENVIRONMENT2 = IO_RAW_SAMPLES_WITH_ENVIRONMENT1[!IO_INDIC_OTHER_OCEAN]

# No geometry
IO_INDIC_NO_GEOMETRY = is.na(IO_RAW_SAMPLES_WITH_ENVIRONMENT2$geometry)
IO_RAW_SAMPLES_WITH_ENVIRONMENT3 = IO_RAW_SAMPLES_WITH_ENVIRONMENT2[!IO_INDIC_NO_GEOMETRY]

## Atlantic Ocean ####
AO_RAW_SAMPLES_WITH_ENVIRONMENT = fread("../inputs/data/AAD ATL Chassot 2021.txt")

# TEMP
# Replace ";" by " " in geometry
indic_semi_colon =  grep(";", AO_RAW_SAMPLES_WITH_ENVIRONMENT$geometry)
AO_RAW_SAMPLES_WITH_ENVIRONMENT[indic_semi_colon, geometry := gsub(";", " ", geometry)]

indic_comma = grep("^POINT.*,", AO_RAW_SAMPLES_WITH_ENVIRONMENT$geometry)
AO_RAW_SAMPLES_WITH_ENVIRONMENT[indic_comma, geometry := gsub(",", ".", geometry)]

# Note: error in geometries (fish from WPO)
#POLYGON ((108 15. 115 15.115 20.-175.20.-175 -25.155 -25.155 -28.15.153.57 -28.15.105.82 -6.38.105.07 -6.25.104.5 -5.52))

# No information on origin of the fish
AO_INDIC_NO_OCEAN_INFORMATION = is.na(AO_RAW_SAMPLES_WITH_ENVIRONMENT$ocean_code)
AO_RAW_SAMPLES_WITH_ENVIRONMENT1 = AO_RAW_SAMPLES_WITH_ENVIRONMENT[!AO_INDIC_NO_OCEAN_INFORMATION]

# No geometry
AO_INDIC_NO_GEOMETRY = is.na(AO_RAW_SAMPLES_WITH_ENVIRONMENT1$geometry)
AO_RAW_SAMPLES_WITH_ENVIRONMENT2 = AO_RAW_SAMPLES_WITH_ENVIRONMENT1[!AO_INDIC_NO_GEOMETRY]

# Other data sets ####

IO_EMOTION   = fread("../inputs/data/data_emotion_fl_wt_2009_2014.csv")[c_sp_fao %in% c("BET", "SKJ", "YFT")]
IO_EMOTION[, ocean_code := "IO"]
names(IO_EMOTION) = c("project", "sampling_year", "species_code_fao", "sex", "fork_length", "whole_fish_weight", "ocean_code")
IO_EMOTION[sex == "", sex := NA]

IO_FONTENEAU = fread("../inputs/data/data_fonteneau_fl_wt_2003.csv")
IO_FONTENEAU[, ocean_code := "IO"]
names(IO_FONTENEAU) = c("project", "sampling_year", "species_code_fao", "sex", "fork_length", "whole_fish_weight", "ocean_code")

IO_OTHERS    = fread("../inputs/data/data_iotmdm_fl_wt_1986_2015.csv")[project != "DCF"]
IO_OTHERS[, ocean_code := "IO"]
names(IO_OTHERS) = c("project", "sampling_year", "species_code_fao", "sex", "fork_length", "whole_fish_weight", "ocean_code")

IO_IOTTP     = fread("../inputs/data/data_iottp_fl_wt_2009_2015.csv")
IO_IOTTP[, ocean_code := "IO"]
names(IO_IOTTP) = c("project", "sampling_year", "species_code_fao", "sex", "fork_length", "whole_fish_weight", "ocean_code")
IO_IOTTP[sex == "", sex := NA]

print("Morphometric data read")
