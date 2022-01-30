
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

# Map of fish samples
# Long/lat represent the centroids of the positions
SAMPLES_MAP = 
BaseMap +
  geom_sf(data = LONGHURST, fill = NA) +
  geom_sf_text(data = LONGHURST_CENTROIDS_FOR_MAP, aes(label = ProvCode), show.legend = FALSE, check_overlap = TRUE) +
  geom_sf(data = SAMPLES_WITH_ENVIRONMENT_SF, aes(shape = species_code_fao, colour = species_code_fao), alpha = 0.2) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  scale_x_continuous(limits = c(-27, 80)) +
  scale_y_continuous(limits = c(-30, 27)) +
  scale_color_manual(values = SPECIES_COL_SHAPE$FILL) +
  scale_shape_manual(values = SPECIES_COL_SHAPE$SHAPE) + 
  #scale_alpha( values = 0.9) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(x = "Longitude", y = "Latitude")

ggsave("../outputs/charts/SAMPLES_MAP.png", SAMPLES_MAP, width = 12, height = 4.5/8*12)
