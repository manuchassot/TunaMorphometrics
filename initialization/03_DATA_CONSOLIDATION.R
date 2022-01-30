
print("Consolidate morphometric data...")

# CONSOLIDATE DCF/EUMAP DATA SETS ####

# Combine both data sets
RAW_SAMPLES_WITH_ENVIRONMENT = rbindlist(list(AO_RAW_SAMPLES_WITH_ENVIRONMENT2, IO_RAW_SAMPLES_WITH_ENVIRONMENT3))

# TEMP: REMOVE samples with obvious errors

# 1- Remove fish with duplicates of fish identifiers
FISH_IDENTIFIER_TWO_OCEANS = data.table(read.xlsx("../inputs/data/FISH_IDENTIFIER_TWO_OCEANS.xlsx", colNames = TRUE))

RAW_SAMPLES_WITH_ENVIRONMENT = RAW_SAMPLES_WITH_ENVIRONMENT[!(fish_identifier %in% FISH_IDENTIFIER_TWO_OCEANS$fish_identifier)]

# 2- Remove rows of environment duplicated in MULTIPOINT

FISH_IDENTIFIER_MIX_MULTI_SINGLE_POINT = data.table(read.xlsx("../inputs/data/FISH_IDENTIFIER_MIX_MULTI_SINGLE_POINT.xlsx"))

CORRECTED_SAMPLES = RAW_SAMPLES_WITH_ENVIRONMENT[(fish_identifier %in% FISH_IDENTIFIER_MIX_MULTI_SINGLE_POINT$fish_identifier & substr(geometry, start = 1, stop = 5) == "POINT")]

RAW_SAMPLES_WITH_ENVIRONMENT = RAW_SAMPLES_WITH_ENVIRONMENT[!(fish_identifier %in% FISH_IDENTIFIER_MIX_MULTI_SINGLE_POINT$fish_identifier)]

RAW_SAMPLES_WITH_ENVIRONMENT = rbindlist(list(RAW_SAMPLES_WITH_ENVIRONMENT, CORRECTED_SAMPLES))

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

# Update vessel storage mode
RAW_SAMPLES_WITH_ENVIRONMENT[vessel_storage_mode == "brine", vessel_storage_mode := "Brine"]
RAW_SAMPLES_WITH_ENVIRONMENT[vessel_name %in% c("ARTZA", "CAP SAINTE MARIE", "GLENAN", "TORRE ITALIA"), vessel_storage_mode := "Brine"]
RAW_SAMPLES_WITH_ENVIRONMENT[is.na(vessel_storage_mode), vessel_storage_mode := "Unknow"]

# Update devices
RAW_SAMPLES_WITH_ENVIRONMENT[measuring_device_1 == "Calliper", measuring_device_1 := "calliper"]
RAW_SAMPLES_WITH_ENVIRONMENT[measuring_device_2 == "Calliper", measuring_device_2 := "calliper"]

# Format fishing dates
RAW_SAMPLES_WITH_ENVIRONMENT[, fishing_date_min := as.POSIXct(fishing_date_min)]
RAW_SAMPLES_WITH_ENVIRONMENT[, fishing_date_max := as.POSIXct(fishing_date_max)]
RAW_SAMPLES_WITH_ENVIRONMENT[, fishing_date := as.POSIXct(fishing_date)]

# Add fields
RAW_SAMPLES_WITH_ENVIRONMENT[, stock := paste(ocean_code, species_code_fao, sep = " | ")]
RAW_SAMPLES_WITH_ENVIRONMENT[, species_code_fao := as.factor(species_code_fao)]
RAW_SAMPLES_WITH_ENVIRONMENT[, sex := as.factor(sex)]
RAW_SAMPLES_WITH_ENVIRONMENT[species_code_fao == "BET", `:=` (species_english_name = "Bigeye tuna", species_scientific_name = "Thunnus obesus")]
RAW_SAMPLES_WITH_ENVIRONMENT[species_code_fao == "SKJ", `:=` (species_english_name = "Skipjack tuna", species_scientific_name = "Katsuwonus pelamis")]
RAW_SAMPLES_WITH_ENVIRONMENT[species_code_fao == "YFT", `:=` (species_english_name = "Yellowfin tuna", species_scientific_name = "Thunnus albacares")]
RAW_SAMPLES_WITH_ENVIRONMENT[ocean_code == "AO", ocean := "Atlantic Ocean"]
RAW_SAMPLES_WITH_ENVIRONMENT[ocean_code == "IO", ocean := "Indian Ocean"]

# Consolidate fishing dates

# Remove fishing_date_min and fishing_date_max if equal to fishing_date
#SAMPLES_WITH_ENVIRONMENT[fishing_date == fishing_date_min & fishing_date == fishing_date_max, `:=` (fishing_date_min = NA, fishing_date_max = NA)]

# Remove fishing_date_min and fishing_date_max if fishing_date not null
RAW_SAMPLES_WITH_ENVIRONMENT[!is.na(fishing_date), `:=` (fishing_date_min = NA, fishing_date_max = NA)]

# Compute "average" date

RAW_SAMPLES_WITH_ENVIRONMENT[!is.na(fishing_date), fishing_date_avg := as.POSIXct(min(fishing_date) + difftime(max(fishing_date), min(fishing_date), units = 'days')/2, format = "%Y-%m-%d"), by = .(fish_identifier)]

RAW_SAMPLES_WITH_ENVIRONMENT[is.na(fishing_date), fishing_date_avg := as.POSIXct(min(fishing_date_min) + difftime(max(fishing_date_max), min(fishing_date_min), units = 'days')/2, format = "%Y-%m-%d"), by = .(fish_identifier)]

# Add fishing date range
RAW_SAMPLES_WITH_ENVIRONMENT[is.na(fishing_date), fishing_date_range := as.numeric(difftime(fishing_date_max, fishing_date_min, units = "days"))]

RAW_SAMPLES_WITH_ENVIRONMENT[!is.na(fishing_date), fishing_date_range := 0]

## SPLIT DATA SETS INTO THREE COMPONENTS ####

# Single geometry

FISH_IDENTIFIERS_SINGLE_ENVIRONMENT = RAW_SAMPLES_WITH_ENVIRONMENT[, .N, by = .(fish_identifier)][N == 1, fish_identifier]

RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE = RAW_SAMPLES_WITH_ENVIRONMENT[fish_identifier %in% FISH_IDENTIFIERS_SINGLE_ENVIRONMENT]

## Single geometry of type POINT

RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE_POINT = RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE[!grep("MULTI", RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE$geometry)]

RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE_POINT[, long_centroid := get_centroid(geometry)[1], by = .(fish_identifier)]

RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE_POINT[, lat_centroid := get_centroid(geometry)[2], by = .(fish_identifier)]

SAMPLES_WITH_ENVIRONMENT_SINGLE_POINT = RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE_POINT[, -c("quadrant", "fishing_date", "fishing_date_min", "fishing_date_max", "geometry")]

## Single geometry of type MULTIPOINT

RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE_MULTIPOINT = RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE[grep("MULTI", RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE$geometry)]

RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE_MULTIPOINT[, long_centroid := mean(get_centroid(geometry)[, 1]), by = .(fish_identifier)]

RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE_MULTIPOINT[, lat_centroid  := mean(get_centroid(geometry)[, 2]), by = .(fish_identifier)]

SAMPLES_WITH_ENVIRONMENT_SINGLE_MULTIPOINT = RAW_SAMPLES_WITH_ENVIRONMENT_SINGLE_MULTIPOINT[, -c("quadrant", "fishing_date", "fishing_date_min", "fishing_date_max", "geometry")]

# Multiple geometries

RAW_SAMPLES_WITH_ENVIRONMENT_MULTIPLE = RAW_SAMPLES_WITH_ENVIRONMENT[!fish_identifier %in% FISH_IDENTIFIERS_SINGLE_ENVIRONMENT]

## Multiple geometry of type POINT

#FISH_IDENTIFIERS_MULTIPLE_MULTIPOINT = RAW_SAMPLES_WITH_ENVIRONMENT_MULTIPLE[grep("MULTI", RAW_SAMPLES_WITH_ENVIRONMENT_MULTIPLE$geometry), fish_identifier]

RAW_SAMPLES_WITH_ENVIRONMENT_MULTIPLE_POINTS = RAW_SAMPLES_WITH_ENVIRONMENT_MULTIPLE[substr(geometry, 1, 5) == "POINT"]

RAW_SAMPLES_WITH_ENVIRONMENT_MULTIPLE_POINTS[, long_centroid := get_centroid(geometry)[1], by = .(fish_identifier, geometry)]

RAW_SAMPLES_WITH_ENVIRONMENT_MULTIPLE_POINTS[, lat_centroid := get_centroid(geometry)[2], by = .(fish_identifier, geometry)]

RAW_SAMPLES_WITH_ENVIRONMENT_MULTIPLE_POINTS[, fishing_date_range := as.numeric(difftime(max(fishing_date), min(fishing_date), units = "days")), by = .(fish_identifier)]

SAMPLES_WITH_ENVIRONMENT_MULTIPLE_POINTS = RAW_SAMPLES_WITH_ENVIRONMENT_MULTIPLE_POINTS[, .(sea_surface_temp = mean(sea_surface_temp, na.rm = TRUE),
fishing_mode = fifelse(all(is.na(fishing_mode)), "UNK",
                       fifelse(all(fishing_mode == "FSC"), "FSC",
                                   fifelse(all(fishing_mode == "DFAD"), "DFAD",
                                           "MIX"))),
fishing_date_avg = as.POSIXct(min(fishing_date_avg) + difftime(max(fishing_date_avg), min(fishing_date_avg), units = 'days')/2, format = "%Y-%m-%d"), long_centroid = mean(long_centroid), lat_centroid = mean(lat_centroid)), by = .(fish_identifier, fish_sampling_date, project, species_code_fao, fork_length, measuring_device_1, first_dorsal_length, measuring_device_2, whole_fish_weight, gutted_fish_weight, measuring_device_4, sex, ocean_code, vessel_code, vessel_name, gear_code, landing_site, landing_date, vessel_storage_mode, sampling_year, landing_year, stock, species_english_name, species_scientific_name, ocean, fishing_date_range)]

## DATA SET WITH ENVIRONMENT ####

SAMPLES_WITH_ENVIRONMENT = rbindlist(list(SAMPLES_WITH_ENVIRONMENT_SINGLE_MULTIPOINT, SAMPLES_WITH_ENVIRONMENT_SINGLE_POINT, SAMPLES_WITH_ENVIRONMENT_MULTIPLE_POINTS), use.names = TRUE, fill = TRUE) 

# Add fishing year and quarter
SAMPLES_WITH_ENVIRONMENT[, fishing_year := year(fishing_date_avg)]
SAMPLES_WITH_ENVIRONMENT[, fishing_quarter := quarter(fishing_date_avg)]
SAMPLES_WITH_ENVIRONMENT[fishing_quarter == 1, fishing_year_quarter := fishing_year + 0.125]
SAMPLES_WITH_ENVIRONMENT[fishing_quarter == 2, fishing_year_quarter := fishing_year + 0.375]
SAMPLES_WITH_ENVIRONMENT[fishing_quarter == 3, fishing_year_quarter := fishing_year + 0.625]
SAMPLES_WITH_ENVIRONMENT[fishing_quarter == 4, fishing_year_quarter := fishing_year + 0.875]

# Add Longhurst provinces

sf::sf_use_s2(FALSE)

LONGHURST_RAW = st_read("../inputs/shapes/Longhurst_world_v4_2010.shp")

SAMPLES_WITH_ENVIRONMENT_SF = st_as_sf(SAMPLES_WITH_ENVIRONMENT, coords = c("long_centroid", "lat_centroid"))
st_crs(SAMPLES_WITH_ENVIRONMENT_SF) = st_crs(LONGHURST_RAW)

SAMPLES_WITH_ENVIRONMENT_SF = st_join(SAMPLES_WITH_ENVIRONMENT_SF, LONGHURST_RAW)

# DATA SET WITHOUT ENVIRONMENT ####

# Includes fish samples without spatial information

AO_EU_DATASET = unique(RAW_SAMPLES_WITH_ENVIRONMENT[ocean_code == "AO", .(ocean_code, project, sampling_year, species_code_fao, sex, fork_length, whole_fish_weight)])

IO_EU_DATASET = unique(RAW_SAMPLES_WITH_ENVIRONMENT[ocean_code == "IO", .(ocean_code, project, sampling_year, species_code_fao, sex, fork_length, whole_fish_weight)])

FULL_DATASET = rbindlist(list(AO_EU_DATASET, IO_IOT_NO_GEOMETRY, IO_EU_DATASET, IO_EMOTION, IO_FONTENEAU, IO_OTHERS, IO_IOTTP), use.names = TRUE, fill = TRUE)

print("Morphometric data consolidated!")