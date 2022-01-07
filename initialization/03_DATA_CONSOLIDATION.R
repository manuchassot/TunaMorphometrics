
# Combine both data sets
RAW_SAMPLES_WITH_ENVIRONMENT = rbindlist(list(AO_RAW_SAMPLES_WITH_ENVIRONMENT, IO_RAW_SAMPLES_WITH_ENVIRONMENT))

# Format sampling date
RAW_SAMPLES_WITH_ENVIRONMENT[, fish_sampling_date := as.POSIXct(fish_sampling_date)] 

# Format landing date
RAW_SAMPLES_WITH_ENVIRONMENT[, landing_date := as.POSIXct(landing_date)]

# Add landing year
RAW_SAMPLES_WITH_ENVIRONMENT[, landing_year := year(landing_date)]

# Update fishing mode
RAW_SAMPLES_WITH_ENVIRONMENT[fishing_mode == "NA", fishing_mode := NA]

# Format fishing dates
RAW_SAMPLES_WITH_ENVIRONMENT[, fishing_date_min := as.POSIXct(fishing_date_min)]
RAW_SAMPLES_WITH_ENVIRONMENT[, fishing_date_max := as.POSIXct(fishing_date_max)]
RAW_SAMPLES_WITH_ENVIRONMENT[, fishing_date := as.POSIXct(fishing_date)]

# Update fishing date
RAW_SAMPLES_WITH_ENVIRONMENT[!is.na(fishing_date), `:=` (fishing_date_min = NA, fishing_date_max = NA)]

# Compute "average" date when only min and max are available
RAW_SAMPLES_WITH_ENVIRONMENT[is.na(fishing_date), fishing_date_avg := format(fishing_date_min + difftime(fishing_date_max, fishing_date_min, units = 'days')/2, "%Y-%m-%d")]

RAW_SAMPLES_WITH_ENVIRONMENT[!is.na(fishing_date), fishing_date_avg := format(min(fishing_date) + difftime(max(fishing_date), min(fishing_date), units = 'days')/2, "%Y-%m-%d"), by = .(fish_identifier)]

# Add fishing date range
RAW_SAMPLES_WITH_ENVIRONMENT[!is.na(fishing_date), fishing_date_range := ]

# Update vessel storage mode
RAW_SAMPLES_WITH_ENVIRONMENT[vessel_storage_mode == "brine", vessel_storage_mode := "Brine"]
RAW_SAMPLES_WITH_ENVIRONMENT[vessel_name %in% c("ARTZA", "CAP SAINTE MARIE", "GLENAN", "TORRE ITALIA"), vessel_storage_mode := "Brine"]



# Rename headers
SAMPLES_WITH_ENVIRONMENT = RAW_SAMPLES_WITH_ENVIRONMENT[, .(FISH_ID = fish_identifier, SAMPLING_DATE = fish_sampling_date, PROJECT = project, SPECIES_CODE = species_code_fao, PDL_DEVICE = measuring_device_1, PDL = first_dorsal_length, FL_DEVICE = measuring_device_2, FL = fork_length, RW_device = measuring_device_4, RW = whole_fish_weight, SEX = sex, GEAR_CODE = gear_code, SCHOOL_TYPE = fishing_mode, FISHING_DATE = fishing_date, GEOM = geometry)]




# Convert to simple feature
RAW_SAMPLES_WITH_ENVIRONMENT_SF = st_as_sf(RAW_SAMPLES_WITH_ENVIRONMENT, wkt = "geometry", crs = 4326)


toto1_SF = st_as_sf(toto1, wkt = "GEOM", crs = 4326)

TOTO = 
  toto1_SF[1:10000, ] %>% group_by(FISH_ID) %>% summarise()




#toto1[, SF := st_union(rgeos::readWKT(GEOM)), keyby = .(FISH_ID)]

