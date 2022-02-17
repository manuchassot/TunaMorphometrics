
print("Consolidate morphometric data...")

# CONSOLIDATE DCF/EUMAP DATA SETS ####

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

SAMPLES_WITH_ENVIRONMENT_NO_LOCATION = SAMPLES_WITH_ENVIRONMENT[geometry = "WKT"]

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