
# Reading the data ####

AO_RAW_SAMPLES_WITH_ENVIRONMENT = fread("../inputs/data/AAD ATL Chassot 2021.txt")[!is.na(geometry)]

# Replace ";" by " " in geometry
indic_semi_colon =  grep(";", AO_RAW_SAMPLES_WITH_ENVIRONMENT$geometry)
AO_RAW_SAMPLES_WITH_ENVIRONMENT[indic_semi_colon, geometry := gsub(";", " ", geometry)]

indic_comma = grep(",", AO_RAW_SAMPLES_WITH_ENVIRONMENT$geometry)

AO_RAW_SAMPLES_WITH_ENVIRONMENT[indic_comma & !(grep("MULTIPOINT", geometry)), geometry := gsub(",", ".", geometry)]

# Convert to simple feature
AO_RAW_SAMPLES_WITH_ENVIRONMENT_SF = st_as_sf(AO_RAW_SAMPLES_WITH_ENVIRONMENT, wkt = "geometry", crs = 4326)
