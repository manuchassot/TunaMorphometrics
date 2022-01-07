
# Data collected as per EU (DCF and EUMAP) ####

## Indian Ocean ####
IO_RAW_SAMPLES_WITH_ENVIRONMENT = fread("../inputs/data/AAD IND Chassot 2021.txt")[!is.na(geometry) & geometry != "NA" & ocean_code == "IO"]

## Atlantic Ocean ####
AO_RAW_SAMPLES_WITH_ENVIRONMENT = fread("../inputs/data/AAD ATL Chassot 2021.txt")[!is.na(geometry)]

# TEMP
# Replace ";" by " " in geometry
indic_semi_colon =  grep(";", AO_RAW_SAMPLES_WITH_ENVIRONMENT$geometry)
AO_RAW_SAMPLES_WITH_ENVIRONMENT[indic_semi_colon, geometry := gsub(";", " ", geometry)]

indic_comma = grep("^POINT.*,", AO_RAW_SAMPLES_WITH_ENVIRONMENT$geometry)
AO_RAW_SAMPLES_WITH_ENVIRONMENT[indic_comma, geometry := gsub(",", ".", geometry)]

# Note: error in geometries (fish from WPO)
#POLYGON ((108 15. 115 15.115 20.-175.20.-175 -25.155 -25.155 -28.15.153.57 -28.15.105.82 -6.38.105.07 -6.25.104.5 -5.52))

