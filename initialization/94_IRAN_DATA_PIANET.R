

IOTP = data.table(read.xlsx("../inputs/data/IO_IRAN_IOTP_PIANET.xlsx"))

IOTP[, SAMPLE_SIZE := .N, by = .(species_code_fao)]

IOTP[, SPECIES_SAMPLE_SIZE := paste(species_code_fao, " (N = " , prettyNum(SAMPLE_SIZE, big.mark = ","), ")", sep = "")]

# IRAN samples
IOTP_SAMPLES_PLOT =
ggplot(IOTP, aes(x = fork_length, y = whole_fish_weight, col = species_code_fao)) +
  geom_point(shape = 3, size = 0.8) +
  theme_bw() +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)", title = "Samples from I.R. Iran") +
  theme(strip.background = element_rect(fill = "white"), strip.text.x = element_text(size = 12), legend.position = "none") + 
  facet_wrap(~SPECIES_SAMPLE_SIZE, scales = "free")

ggsave("../outputs/charts/SAMPLES/RAW_LW_IOTP_IRAN.png", IOTP_SAMPLES_PLOT, width = 8, height = 8)

# Comparison with IOT for SKJ

IOTP_IOT_SAMPLES_SKJ = rbindlist(list(
  SAMPLES_WITH_ENVIRONMENT[ocean_code == "IO" & species_code_fao == "SKJ", .(fork_length, whole_fish_weight, source = "IOT")],
  IOTP[species_code_fao == "SKJ", .(fork_length, whole_fish_weight, source = "IOTP")]))

IOTP_IOT_SAMPLES_SKJ =
ggplot(data = IOTP_IOT_SAMPLES_SKJ, aes(x = fork_length, y = whole_fish_weight, color = source)) +
  geom_point(shape = 3, size = 0.8) +
  theme_bw() +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)") +
  theme(legend.position = "bottom", legend.title = element_blank())
  
ggsave("../outputs/charts/SAMPLES/RAW_LW_IOTP_IOT_SKJ.png", IOTP_IOT_SAMPLES_SKJ, width = 5, height = 5)
