
print("Consolidate morphometric data...")

# Format capture date range
SAMPLES_WITH_ENVIRONMENT[, capture_date_range := as.numeric(capture_date_range)]

# Temporary corrections
SAMPLES_WITH_ENVIRONMENT[long == 18.5, ocean_code := "AO"]
SAMPLES_WITH_ENVIRONMENT[long == 4.75, ocean_code := "AO"]

# Add ocean
SAMPLES_WITH_ENVIRONMENT[ocean_code == "IO", ocean := "Indian Ocean"]
SAMPLES_WITH_ENVIRONMENT[ocean_code == "AO", ocean := "Atlantic Ocean"]

# Add species English and scientific names
SAMPLES_WITH_ENVIRONMENT[species_code_fao == "BET", `:=` (species_english_name = "Bigeye tuna", scientific_name = "Thunnus obesus")]
SAMPLES_WITH_ENVIRONMENT[species_code_fao == "SKJ", `:=` (species_english_name = "Skipjack tuna", scientific_name = "Katsuwonus pelamis")]
SAMPLES_WITH_ENVIRONMENT[species_code_fao == "YFT", `:=` (species_english_name = "Yellowfin tuna", scientific_name = "Thunnus albacares")]

# Add sampling year
SAMPLES_WITH_ENVIRONMENT[, sampling_year := year(organism_sampling_date)]

# Add landing year
SAMPLES_WITH_ENVIRONMENT[, landing_year := year(landing_date)]

# Add capture year
SAMPLES_WITH_ENVIRONMENT[, capture_year := year(capture_date_avg)]

# Add capture quarter
SAMPLES_WITH_ENVIRONMENT[, capture_quarter := quarter(capture_date_avg)]
SAMPLES_WITH_ENVIRONMENT[capture_quarter == 1, capture_year_quarter := capture_year + 0.125]
SAMPLES_WITH_ENVIRONMENT[capture_quarter == 2, capture_year_quarter := capture_year + 0.375]
SAMPLES_WITH_ENVIRONMENT[capture_quarter == 3, capture_year_quarter := capture_year + 0.625]
SAMPLES_WITH_ENVIRONMENT[capture_quarter == 4, capture_year_quarter := capture_year + 0.875]

# Add longitude and latitude of centroids

# MULTIPLE POINTS

SAMPLES_WITH_ENVIRONMENT_MULTIPOINT = SAMPLES_WITH_ENVIRONMENT[geom == "MULTIPOINT"][!grep("WKT", geom_text)]

SAMPLES_WITH_ENVIRONMENT_MULTIPOINT[, long_centroid := mean(get_centroid(geom_text)[, 1]), by = .(organism_identifier)]
SAMPLES_WITH_ENVIRONMENT_MULTIPOINT[, lat_centroid := mean(get_centroid(geom_text)[, 2]), by = .(organism_identifier)]

# POINTS

SAMPLES_WITH_ENVIRONMENT_POINT = SAMPLES_WITH_ENVIRONMENT[geom == "POINT"]

SAMPLES_WITH_ENVIRONMENT_POINT[, long_centroid := get_centroid(geom_text)[1], by = .(organism_identifier)]
SAMPLES_WITH_ENVIRONMENT_POINT[, lat_centroid := get_centroid(geom_text)[2], by = .(organism_identifier)]

SAMPLES_WITH_ENVIRONMENT_LOCATION = rbindlist(list(SAMPLES_WITH_ENVIRONMENT_MULTIPOINT, SAMPLES_WITH_ENVIRONMENT_POINT))

SAMPLES_WITH_ENVIRONMENT_NO_LOCATION = SAMPLES_WITH_ENVIRONMENT[geom %in% c("WKT_AO", "WKT_IO")]

# Add Longhurst provinces when location is available

sf::sf_use_s2(FALSE)

LONGHURST_RAW = st_read("../inputs/shapes/Longhurst_world_v4_2010.shp")

SAMPLES_WITH_ENVIRONMENT_LOCATION_SF = st_as_sf(SAMPLES_WITH_ENVIRONMENT_LOCATION, coords = c("long_centroid", "lat_centroid"))
st_crs(SAMPLES_WITH_ENVIRONMENT_LOCATION_SF) = st_crs(LONGHURST_RAW)

SAMPLES_WITH_ENVIRONMENT_LOCATION_SF = st_join(SAMPLES_WITH_ENVIRONMENT_LOCATION_SF, LONGHURST_RAW)

SAMPLES_WITH_ENVIRONMENT_LOCATION_DT = as.data.table(SAMPLES_WITH_ENVIRONMENT_LOCATION_SF)[, -c("geometry")] 

SAMPLES_WITH_ENVIRONMENT_NO_LOCATION[, `:=` (ProvCode = NA, ProvDescr = NA)]

# Produce the final data set 

TUNA_SAMPLES = rbindlist(list(SAMPLES_WITH_ENVIRONMENT_LOCATION_DT, SAMPLES_WITH_ENVIRONMENT_NO_LOCATION))

# Temp (to check with Nat)
TUNA_SAMPLES[first_dorsal_length>100, first_dorsal_length := NA]

# Remove samples from ITOP (not enough knowledge on the protocol, origins, etc.)
TUNA_SAMPLES = TUNA_SAMPLES[project != "ITOP", ]

# Select only tunas caught with purse seine
TUNA_SAMPLES = TUNA_SAMPLES[gear_code == "PS", ]

# Remove obvious errors in IO SKJ weights
TUNA_SAMPLES[organism_identifier %in% c("SKJ_20210812_21", "SKJ_20210727_17", "IOT_2016_0405", "IOT_2016_0422", "IOT_2974", "IOT_2454", "IOT2014-016", "IOT_0247", "IOT_3363", "IOT_3366", "IOT_3672", "IOT_3755", "SKJ-09.M1.L17.7", "SKJ-09.M1.L25.23", "SKJ-09.M2.L10.16", "IOT_2938", "SA01893", "DCF-AO-1280", "SKJ-09.M3.L22.4", "IOT_2016_0414", "IOT_3460", "SKJ_20210916_17", "SKJ_20210916_39", "IOT_3128", "IOT_0588", "IOT_3685", "SKJ_20210824_30", "SKJ_20210824_4", "SKJ_20210907_19", "SKJ_20210826_1", "IOT_2016_0416", "IOT_2016_0423", "IOT_2016_0431", "IOT_1033", "SKJ_20210916_31", "IOT_0613", "SKJ_20210701_1", "IOT_2016_0442", "IOT_2494", "IOT_2508", "IOT_2016_0420", "IOT_2016_0458", "IOT_2016_0488", "IOT_0481", "IOT_3365", "IOT_2490"), whole_weight_kg := NA]

# Remove obvious errors in IO SKJ lengths
TUNA_SAMPLES[organism_identifier %in% c("SKJ_20210701_14"), whole_weight_kg:= NA]

TUNA_SAMPLES[organism_identifier == "SA0031", first_dorsal_length := NA]
TUNA_SAMPLES[organism_identifier == "SA01665", first_dorsal_length := NA]
TUNA_SAMPLES[organism_identifier == "SKJ_20210503_11", first_dorsal_length := NA]
TUNA_SAMPLES[organism_identifier == "SA0904", first_dorsal_length := NA]
TUNA_SAMPLES[organism_identifier == "SA0624", first_dorsal_length := NA]
TUNA_SAMPLES[organism_identifier == "SA0782", first_dorsal_length := NA]
TUNA_SAMPLES[organism_identifier == "SA0773", first_dorsal_length := NA]
TUNA_SAMPLES[organism_identifier == "SA0777", first_dorsal_length := NA]

TUNA_SAMPLES[organism_identifier == "SKJ_20210701_14", first_dorsal_length := 19] #assumed instead of 29

TUNA_SAMPLES[organism_identifier == "SA01132", fork_length := NA]

TUNA_SAMPLES[organism_identifier == "SA01034", fork_length := NA]

TUNA_SAMPLES[organism_identifier == "IOT_5065", fork_length := NA]  # very small FL
TUNA_SAMPLES[organism_identifier == "IOT_5066", fork_length := NA]  # very small FL
TUNA_SAMPLES[organism_identifier == "IOT_5067", fork_length := NA]  # very small FL
TUNA_SAMPLES[organism_identifier == "IOT_5011", fork_length := NA]  # inconsistent
TUNA_SAMPLES[organism_identifier == "IOT_4675", fork_length := NA]  # inconsistent
TUNA_SAMPLES[organism_identifier == "IOT_4871", fork_length := NA]  # inconsistent
TUNA_SAMPLES[organism_identifier == "IOT_4585", fork_length := NA]  # inconsistent

# Remove obvious errors in AO SKJ weights
TUNA_SAMPLES[organism_identifier == "DCF-AO-1107", whole_weight_kg := NA]
TUNA_SAMPLES[organism_identifier == "DCF-AO-1372", whole_weight_kg := NA]

# Remove obvious errors in IO YFT weights
TUNA_SAMPLES[organism_identifier %in% c("DCF6878", "IOT24", "OT11610", "A260", "YFT-09.M2.L5.4","IOT_2016_0305", "YFT-09.M2.L4.12", "DCF0031", "DCF18985", "DCF_18985"), whole_weight_kg := NA]

# Prepare the data for modelling
TUNA_SAMPLES[, log10FL  := log(fork_length, 10)]
TUNA_SAMPLES[, log10RW  := log(whole_weight_kg, 10)]
TUNA_SAMPLES[, Quarter  := factor(capture_quarter)]
TUNA_SAMPLES[, Province := factor(ProvCode)]
TUNA_SAMPLES[, SchoolType := factor(aggregation)]
TUNA_SAMPLES[, Sex := factor(sex)]
TUNA_SAMPLES[, log10FDL  := log(first_dorsal_length, 10)]

print("Morphometric data consolidated!")

