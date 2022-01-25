
print("Consolidate morphometric data...")

# CONSOLIDATE DCF/EUMAP DATA SETS ####

# Combine both data sets
RAW_SAMPLES_WITH_ENVIRONMENT = rbindlist(list(AO_RAW_SAMPLES_WITH_ENVIRONMENT1, IO_RAW_SAMPLES_WITH_ENVIRONMENT3))

# Format sampling date
RAW_SAMPLES_WITH_ENVIRONMENT[, fish_sampling_date := as.POSIXct(fish_sampling_date)] 

# Add sampling year
RAW_SAMPLES_WITH_ENVIRONMENT[, sampling_year := year(fish_sampling_date)]

# Format landing date
RAW_SAMPLES_WITH_ENVIRONMENT[, landing_date := as.POSIXct(landing_date)]

# Add landing year
RAW_SAMPLES_WITH_ENVIRONMENT[, landing_year := year(landing_date)]

# Update fishing mode
RAW_SAMPLES_WITH_ENVIRONMENT[fishing_mode == "NA", fishing_mode := NA]

# Update sex
RAW_SAMPLES_WITH_ENVIRONMENT[sex == "H", sex := "M"] # assumption that H is MALE
RAW_SAMPLES_WITH_ENVIRONMENT[sex == "NA", sex := NA]
RAW_SAMPLES_WITH_ENVIRONMENT[sex == "", sex := NA]

# Format fishing dates
RAW_SAMPLES_WITH_ENVIRONMENT[, fishing_date_min := as.POSIXct(fishing_date_min)]
RAW_SAMPLES_WITH_ENVIRONMENT[, fishing_date_max := as.POSIXct(fishing_date_max)]
RAW_SAMPLES_WITH_ENVIRONMENT[, fishing_date := as.POSIXct(fishing_date)]

# Update fishing date with average fishing date when missing
#RAW_SAMPLES_WITH_ENVIRONMENT[is.na(fishing_date) & !is.na(fishing_date_min)] 

RAW_SAMPLES_WITH_ENVIRONMENT[!is.na(fishing_date), `:=` (fishing_date_min = NA, fishing_date_max = NA)]

# Compute "average" date when only min and max are available
RAW_SAMPLES_WITH_ENVIRONMENT[is.na(fishing_date), fishing_date_avg := as.Date(fishing_date_min + difftime(fishing_date_max, fishing_date_min, units = "days")/2, format = "%Y-%m-%d")]

RAW_SAMPLES_WITH_ENVIRONMENT[!is.na(fishing_date), fishing_date_avg := as.POSIXct(min(fishing_date) + difftime(max(fishing_date), min(fishing_date), units = 'days')/2, format = "%Y-%m-%d"), by = .(fish_identifier)]

RAW_SAMPLES_WITH_ENVIRONMENT[, fishing_date_avg := as.POSIXct(fishing_date_avg)]

# Add fishing date range
RAW_SAMPLES_WITH_ENVIRONMENT[is.na(fishing_date), fishing_date_range := as.numeric(difftime(fishing_date_max, fishing_date_min, units = "days"))]
RAW_SAMPLES_WITH_ENVIRONMENT[!is.na(fishing_date), fishing_date_range := as.numeric(difftime(max(fishing_date), min(fishing_date), units = "days")), by = .(fish_identifier)]

# Update vessel storage mode
RAW_SAMPLES_WITH_ENVIRONMENT[vessel_storage_mode == "brine", vessel_storage_mode := "Brine"]
RAW_SAMPLES_WITH_ENVIRONMENT[vessel_name %in% c("ARTZA", "CAP SAINTE MARIE", "GLENAN", "TORRE ITALIA"), vessel_storage_mode := "Brine"]
RAW_SAMPLES_WITH_ENVIRONMENT[is.na(vessel_storage_mode), vessel_storage_mode := "Unknow"]

# Update devices
RAW_SAMPLES_WITH_ENVIRONMENT[measuring_device_1 == "Calliper", measuring_device_1 := "calliper"]
RAW_SAMPLES_WITH_ENVIRONMENT[measuring_device_2 == "Calliper", measuring_device_2 := "calliper"]

# DEFINE DATA SETS ####
AO_EU_DATASET = unique(RAW_SAMPLES_WITH_ENVIRONMENT[ocean_code == "AO", .(ocean_code, project, sampling_year, species_code_fao, sex, fork_length, whole_fish_weight)])

IO_EU_DATASET = unique(RAW_SAMPLES_WITH_ENVIRONMENT[ocean_code == "IO", .(ocean_code, project, sampling_year, species_code_fao, sex, fork_length, whole_fish_weight)])

FULL_DATASET = rbindlist(list(AO_EU_DATASET, IO_EU_DATASET, IO_EMOTION, IO_FONTENEAU, IO_OTHERS, IO_IOTTP), use.names = TRUE, fill = TRUE)

# Consolidate FULL LENGTH_WEIGHT DATA SET 
FULL_DATASET[, stock := paste(ocean_code, species_code_fao, sep = " | ")]
FULL_DATASET[, species_code_fao := as.factor(species_code_fao)]
FULL_DATASET[, sex := as.factor(sex)]
#FULL_DATASET[sex %in% c("", "NA"), sex := NA]
FULL_DATASET[species_code_fao == "BET", `:=` (species_english_name = "Bigeye tuna", species_scientific_name = "Thunnus obesus")]
FULL_DATASET[species_code_fao == "SKJ", `:=` (species_english_name = "Skipjack tuna", species_scientific_name = "Katsuwonus pelamis")]
FULL_DATASET[species_code_fao == "YFT", `:=` (species_english_name = "Yellowfin tuna", species_scientific_name = "Thunnus albacares")]
FULL_DATASET[ocean_code == "AO", ocean := "Atlantic Ocean"]
FULL_DATASET[ocean_code == "IO", ocean := "Indian Ocean"]

# Rename headers
#SAMPLES = unique(RAW_SAMPLES_WITH_ENVIRONMENT[, .(FISH_ID = fish_identifier, SAMPLING_DATE = fish_sampling_date, PROJECT = project, SPECIES_CODE = species_code_fao, FL = fork_length, FL_DEVICE = measuring_device_1, SF = first_dorsal_length, SF_DEVICE = measuring_device_2,  RD = whole_fish_weight, RD_device = measuring_device_4, SEX = sex, GEAR_CODE = gear_code, sCHOOL_TYPE = fifelse(length(unique(fishing_mode)) >1, "MIX", unique(fishing_mode)), FISHING_DATE_AVG = fishing_date_avg)])

# Identify id replicates
ID_REPLICATES = unique(RAW_SAMPLES_WITH_ENVIRONMENT[, .(fish_identifier, species_code_fao, fork_length, first_dorsal_length, whole_fish_weight, gutted_fish_weight, measuring_device_1, measuring_device_2, measuring_device_4)])[, .N, keyby = fish_identifier][N>1, fish_identifier]

SAMPLES_VARIABLES = RAW_SAMPLES_WITH_ENVIRONMENT[!fish_identifier %in% ID_REPLICATES, .(FISHING_DATE_AVG = format(min(fishing_date_avg) + difftime(max(fishing_date_avg), min(fishing_date_avg), units = 'days')/2, "%Y-%m-%d")), keyby = .(FISH_ID = fish_identifier, SAMPLING_DATE = fish_sampling_date, PROJECT = project, SPECIES_CODE = species_code_fao, FL = fork_length, FL_DEVICE = measuring_device_1, SF = first_dorsal_length, SF_DEVICE = measuring_device_2,  RD = whole_fish_weight, RD_device = measuring_device_4, SEX = sex, GEAR_CODE = gear_code)]

# .(SCHOOL_TYPE = fifelse(length(unique(fishing_mode)) == 1, unique(fishing_mode), "MIX"))
# SCHOOL_TYPE
# FISHING_DATE_AVG
# FISHING_DATE_MIN
# FISGING_DATE_MAX

# Convert to simple feature
#RAW_SAMPLES_WITH_ENVIRONMENT_SF = st_as_sf(RAW_SAMPLES_WITH_ENVIRONMENT[1:1000], wkt = "geometry", crs = 4326)

#RAW_SAMPLES_WITH_ENVIRONMENT_SF %>% group_by(FISH_ID)

#toto1_SF = st_as_sf(toto1, wkt = "GEOM", crs = 4326)

#TOTO = toto1_SF[1:10000, ] %>% group_by(FISH_ID) %>% summarise()

#toto1[, SF := st_union(rgeos::readWKT(GEOM)), keyby = .(FISH_ID)]

print("Morphometric data consolidated!")