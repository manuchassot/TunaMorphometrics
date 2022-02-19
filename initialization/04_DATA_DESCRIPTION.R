
# SAMPLING DESIGN ####

## OCEAN ####
N_TOT = TUNA_SAMPLES[, .N]
N_IO  = TUNA_SAMPLES[ocean_code == "IO", .N]
N_AO  = TUNA_SAMPLES[ocean_code == "AO", .N]

N_TOT_GEOREFERENCED = TUNA_SAMPLES[!is.na(ProvCode), .N]
N_IO_GEOREFERENCED = TUNA_SAMPLES[ocean_code == "IO" & !is.na(ProvCode), .N]
N_AO_GEOREFERENCED = TUNA_SAMPLES[ocean_code == "AO" & !is.na(ProvCode), .N]
  
## PROVINCES ####
# Geo-referenced data set
DESIGN_PROVINCE = TUNA_SAMPLES[!is.na(ProvCode), .N, keyby = .(ocean_code, ProvCode)]
DESIGN_PROVINCE[, N_OCEAN := sum(N), by = .(ocean_code)]
DESIGN_PROVINCE[, PERCENT := round(N/N_OCEAN*100, 1)]

## SPECIES ####
N_BET = TUNA_SAMPLES[species_code_fao == "BET", .N]
N_SKJ = TUNA_SAMPLES[species_code_fao == "SKJ", .N]
N_YFT = TUNA_SAMPLES[species_code_fao == "YFT", .N] 

PERCENT_BET = round(N_BET/N_TOT*100, 0)
PERCENT_SKJ = round(N_SKJ/N_TOT*100, 0)
PERCENT_YFT = round(N_YFT/N_TOT*100, 0)

## ARTICLE TABLE ####

SAMPLING_DESIGN_TABLE = TUNA_SAMPLES[, .(N = length(unique(organism_identifier)), LD = paste(min(round(first_dorsal_length), na.rm = TRUE), max(round(first_dorsal_length), na.rm = TRUE), sep = "-"), LF = paste(min(round(fork_length), na.rm = TRUE), max(round(fork_length), na.rm = TRUE), sep = "-"), WR = paste(min(round(whole_weight_kg, 1), na.rm = TRUE), max(round(whole_weight_kg, 1), na.rm = TRUE), sep = "-")), keyby = .(Ocean = ocean, `Species code` = species_code_fao, `Species name` = species_english_name)]

SAMPLING_DESIGN_TABLE_FT =
  SAMPLING_DESIGN_TABLE %>% 
  flextable() %>%
  merge_at(i = 1:3, j = "Ocean", part = "body") %>%
  merge_at(i = 4:6, j = "Ocean", part = "body") %>%
  hline(i = 3, border = fp_border(width = 1)) %>%
  compose(part = "header", j = "LD", value = as_paragraph("L", as_sub("D"))) %>%
  compose(part = "header", j = "LF", value = as_paragraph("L", as_sub("F"))) %>%
  compose(part = "header", j = "WR", value = as_paragraph("W", as_sub("R"))) %>%
  align(part = "header", j = 4:7, align = "center") %>%
  align(part = "body", j = 4:7, align = "right") %>%
  autofit() %>%
  fix_border_issues()

# FISH ORIGIN UNCERTAINTY ####

SAMPLES_CAPTURE_DATE_UNCERTAINTY = 
  ggplot(data = TUNA_SAMPLES, aes(x = capture_date_range, fill = ocean, colour = ocean)) +
  geom_histogram(bins = 30) +
  theme_bw() +
  scale_color_manual(values = OCEAN_COL_SHAPE$OUTLINE) +
  scale_fill_manual(values = OCEAN_COL_SHAPE$FILL) +
  labs(x = "Range of uncertainty (days)", y = "Frequency") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave("../outputs/charts/DESCRIPTION/SAMPLES_CAPTURE_DATE_UNCERTAINTY.png")

# SAMPLES MAP ####

# World map
world_map = rnaturalearth::ne_countries(scale = "small", returnclass = c("sf"))

# Base map
BaseMap = 
  ggplot() +
  geom_sf(data = world_map, size = .2, fill = "darkgrey", col = NA) +
  theme(panel.grid.major = element_line(color = gray(0.9), linetype = "dashed", size = 0.5))
        
# Longhurst provinces
# https://rstudio-pubs-static.s3.amazonaws.com/485396_7d5f60e87225469fb0c0c04684a0cf31.html

LONGHURST = LONGHURST_RAW %>% 
  sf::st_simplify(dTolerance = 0.01) %>% 
  dplyr::group_by(ProvCode, ProvDescr) %>% 
  dplyr::summarise()

# Create sf object with centroids
LONGHURST_CENTROIDS_RAW = st_centroid(LONGHURST)

# Export LONGHURST CENTROIDS
LONGHURST_CENTROIDS =
  LONGHURST_CENTROIDS_RAW %>%
  cbind(., st_coordinates(.)) %>%
  st_set_geometry(NULL) #%>%
  #write_csv(., "../inputs/data/LONGHURST_CENTROIDS.csv")

LONGHURST_CENTROIDS_FOR_MAP =
  LONGHURST_CENTROIDS %>%
  filter(ProvCode %in% c("CNRY", "GUIN", "WTRA", "ETRA", "SATL", "BENG", "EAFR", "ARAB", "MONS", "ISSG")) %>%
  setDT()

# Manually correct positions of text
LONGHURST_CENTROIDS_FOR_MAP[ProvCode == "WTRA", `:=` (X = -26, Y = 0)]
LONGHURST_CENTROIDS_FOR_MAP[ProvCode == "CNRY", `:=` (X = -15, Y = 22)]
LONGHURST_CENTROIDS_FOR_MAP[ProvCode == "ETRA", `:=` (X = -8, Y = -7)]
LONGHURST_CENTROIDS_FOR_MAP[ProvCode == "GUIN", `:=` (X = 7, Y = 3)]

LONGHURST_CENTROIDS_FOR_MAP = st_as_sf(LONGHURST_CENTROIDS_FOR_MAP, coords = c("X", "Y"))
st_crs(LONGHURST_CENTROIDS_FOR_MAP) = st_crs(LONGHURST_RAW)

# Factorize Species
SAMPLES_WITH_ENVIRONMENT_LOCATION_SF$species_code_fao = factor(SAMPLES_WITH_ENVIRONMENT_LOCATION_SF$species_code_fao, levels = c("Bigeye tuna", "Yellowfin tuna", "Skipjack tuna"))

# Map of fish samples
# Long/lat represent the centroids of the positions
SAMPLES_MAP = 
BaseMap +
  geom_sf(data = LONGHURST, fill = NA) +
  geom_sf_text(data = LONGHURST_CENTROIDS_FOR_MAP, aes(label = ProvCode), show.legend = FALSE, check_overlap = TRUE) +
  geom_sf(data = SAMPLES_WITH_ENVIRONMENT_LOCATION_SF, aes(shape = species_code_fao, colour = species_code_fao), alpha = 0.2) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  scale_x_continuous(limits = c(-27, 80)) +
  scale_y_continuous(limits = c(-35, 27)) +
  scale_color_manual(values = SPECIES_COL_SHAPE$FILL) +
  scale_shape_manual(values = SPECIES_COL_SHAPE$SHAPE) + 
  #scale_alpha( values = 0.9) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = "Longitude", y = "Latitude")

ggsave("../outputs/charts/DESCRIPTION/SAMPLES_MAP.png", SAMPLES_MAP, width = 12, height = 4.5/8*12)

