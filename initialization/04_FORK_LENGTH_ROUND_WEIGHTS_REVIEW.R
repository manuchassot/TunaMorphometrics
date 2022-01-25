
print("Initialise review of length-weight parameters for tropical tuna...")

# Literature review on a and b parameters

# FISHBASE ####

# Get FishBase references 
FISHBASE_REF = as.data.table(rfishbase::references())[, .(RefNo, AuthorStd, Year, Title, SourceUnique)]

# Extract length-weight parameters
FISHBASE_LW_RAW = as.data.table(length_weight(species_list = c("Katsuwonus pelamis", "Thunnus obesus", "Thunnus albacares")))[, .(Species, Locality, StockCode, SpecimenType, LengthType = Type, LengthMin, LengthMax, Sex, Number, a, b, SEa, SEb, PopLWRef)]

# Add references
FISHBASE_LW = merge(FISHBASE_LW_RAW, FISHBASE_REF, by.x = "PopLWRef", by.y = "RefNo", all.x = TRUE)[, PopLWRef := NULL]

setnames(FISHBASE_LW, "Species", "SpeciesScientific") 

# Add species code
FISHBASE_LW[SpeciesScientific == "Thunnus obesus",     `:=` (Species = "Bigeye tuna", SpeciesCode = "BET")]
FISHBASE_LW[SpeciesScientific == "Katsuwonus pelamis", `:=` (Species = "Skipjack tuna", SpeciesCode = "SKJ")]
FISHBASE_LW[SpeciesScientific == "Thunnus albacares",  `:=` (Species = "Yellowfin tuna", SpeciesCode = "YFT")]

# Add oceans
FISHBASE_LW[grep("South Carolina", Locality), Ocean := "Atlantic Ocean"]
FISHBASE_LW[grep("Atlantic", Locality), Ocean := "Atlantic Ocean"]
FISHBASE_LW[grep("Dakar", Locality), Ocean := "Atlantic Ocean"]
FISHBASE_LW[grep("Eastern Cape", Locality), Ocean := "Atlantic Ocean"]
FISHBASE_LW[Locality %in% c("West Zone", "Southwest Zone", "Southeastern  coast"), Ocean := "Atlantic Ocean"]
FISHBASE_LW[grep("Brazilian", Title), Ocean := "Atlantic Ocean"]
FISHBASE_LW[grep("Brasil", Title), Ocean := "Atlantic Ocean"]
FISHBASE_LW[grep("Brazil", SourceUnique), Ocean := "Atlantic Ocean"]
FISHBASE_LW[grep("Southern Africa", Title), Ocean := "Atlantic Ocean"] #assumption as fish are from South Africa and could come from the Indian Ocean
FISHBASE_LW[grep("southern Africa", Title), Ocean := "Atlantic Ocean"] #assumption as fish are from South Africa and could come from the Indian Ocean
FISHBASE_LW[grep("Cuba", SourceUnique), Ocean := "Atlantic Ocean"]
FISHBASE_LW[grep("Atlantic", Title), Ocean := "Atlantic Ocean"]

FISHBASE_LW[grep("Oman", Title), Ocean := "Indian Ocean"]
FISHBASE_LW[grep("Madagascar", Locality), Ocean := "Indian Ocean"]
FISHBASE_LW[grep("Indian Ocean", Locality), Ocean := "Indian Ocean"]
FISHBASE_LW[grep("Bali", Locality), Ocean := "Indian Ocean"]
FISHBASE_LW[grep("Western Australia", Locality), Ocean := "Indian Ocean"]

FISHBASE_LW[grep("Davao Gulf", Locality), Ocean := "Western-Central Pacific Ocean"]
FISHBASE_LW[grep("Philippines", Locality), Ocean := "Western-Central Pacific Ocean"]
FISHBASE_LW[grep("New Zealand", Locality), Ocean := "Western-Central Pacific Ocean"]
FISHBASE_LW[grep("New Zealand", Title), Ocean := "Western-Central Pacific Ocean"]
FISHBASE_LW[grep("Japan", Locality), Ocean := "Western-Central Pacific Ocean"]

FISHBASE_LW[grep("Eastern tropical Pacific", Locality), Ocean := "Eastern Pacific Ocean"]
FISHBASE_LW[grep("Hawaii", Locality), Ocean := "Eastern Pacific Ocean"]
FISHBASE_LW[grep("Hawaii", Title), Ocean := "Eastern Pacific Ocean"]
FISHBASE_LW[grep("Hawaii", SourceUnique), Ocean := "Eastern Pacific Ocean"]

# Correction of obvious error in a parameter
FISHBASE_LW[LengthMin == 38, a := a/10]

# Delete sources already collated from literature
FISHBASE_LW = FISHBASE_LW[AuthorStd != "Andrade, H.A.; Campos, R.O."]
FISHBASE_LW = FISHBASE_LW[AuthorStd != "Uchiyama, J.H."]

# Delete records without info on n or with less than 100 fish
#FISHBASE_LW = FISHBASE_LW[!is.na(Number) & Number>=100]

# a parameters for FL -> RW relationship
FISHBASE_PARAMS = FISHBASE_LW[LengthType == "FL", .(SpeciesScientific, Species, SpeciesCode, Sex, Ocean, Number, ForkLengthMin = LengthMin, ForkLengthMax = LengthMax, a = a/1000, b, Source = AuthorStd)][, ORIGIN := "FISHBASE"]

#write.xlsx(FISHBASE_PARAMS, file = "../inputs/data/FISHBASE_PARAMS.xlsx")

# TUNA RFMOS ####

# https://www.iccat.int/Documents/SCRS/Manual/Appendices/Appendix%204%20III%20Trop.pdf

TRFMO_PARAMS = data.table(read.xlsx("../inputs/data/TRFMO_PARAMETERS.xlsx", sheet = "OFFICIAL"))[, ORIGIN := "REFERENCE"]
OTHER_PARAMS = data.table(read.xlsx("../inputs/data/TRFMO_PARAMETERS.xlsx", sheet = "OTHERS"))[, ORIGIN := "OTHERS"]

FL_RW_PARAMS = rbindlist(list(FISHBASE_PARAMS, TRFMO_PARAMS, OTHER_PARAMS), use.names = TRUE)

# Factorize and order the species
FL_RW_PARAMS[, Species := factor(Species, levels = c("Bigeye tuna", "Skipjack tuna", "Yellowfin tuna"))]

# Statistics
FL_RW_PARAMS[, as.list(summary(a*1e5)), keyby = .(Species, SpeciesCode)]
FL_RW_PARAMS[, as.list(summary(b)), keyby = .(Species, SpeciesCode)]

FL_RW_PARAMS_ab_BIPLOT =
ggplot(FL_RW_PARAMS, aes(x = a*1e5, y = b, color = Species, shape = Species)) +
  geom_point(size = 2.5) +
  theme_bw() +
  labs(x = expression(paste("a x 1", e^{-5})), y = "b") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggsave(filename = "../outputs/charts/PRIORS/FL_RW_PARAMS_ab_BIPLOT.png", plot = FL_RW_PARAMS_ab_BIPLOT, width = 8, height = 6)

# Marginal distributions

FL_RW_PARAMS_a_PLOT =
ggplot(FL_RW_PARAMS, aes(x = a*1e5, y = Species, fill = Species)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = expression(paste("a x 1", e^{-5})), y = "") +
  theme(legend.position = "none")

FL_RW_PARAMS_b_PLOT =
ggplot(FL_RW_PARAMS, aes(x = b, y = Species, fill = Species)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "b", y = "") +
  theme(legend.position = "none")

FL_RW_PARAMS_ab_PLOT  = FL_RW_PARAMS_a_PLOT + FL_RW_PARAMS_b_PLOT

FL_RW_PARAMS_ab_PLOT[[2]] = FL_RW_PARAMS_ab_PLOT[[2]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )

ggsave(filename = "../outputs/charts/PRIORS/FL_RW_PARAMS_ab_MARGINAL_PLOTS.png", plot = FL_RW_PARAMS_ab_PLOT, width = 8, height = 4.5)

print("Length-weight parameters for tropical tuna reviewed!")
