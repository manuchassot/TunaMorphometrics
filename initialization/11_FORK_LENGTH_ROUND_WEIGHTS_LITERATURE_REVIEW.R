
print("Initialise review of length-weight parameters for tropical tuna...")

# Literature review on a and b parameters

# FISHBASE ####

# Get FishBase references 
FISHBASE_REF = as.data.table(rfishbase::references())[, .(RefNo, AuthorStd, Year, Title, SourceUnique)]

# Extract length-weight parameters
FISHBASE_LW_RAW = as.data.table(length_weight(species_list = c("Katsuwonus pelamis", "Thunnus obesus", "Thunnus albacares")))[, .(SpeciesScientific = Species, Locality, StockCode, SpecimenType, LengthType = Type, LengthMin, LengthMax, Sex, Number, a, b, SEa, SEb, PopLWRef)]

# Add references
FISHBASE_LW = merge(FISHBASE_LW_RAW, FISHBASE_REF, by.x = "PopLWRef", by.y = "RefNo", all.x = TRUE)[, PopLWRef := NULL]

# Add species code
FISHBASE_LW[SpeciesScientific == "Thunnus obesus",     `:=` (SpeciesName = "Bigeye tuna", SpeciesCode = "BET")]
FISHBASE_LW[SpeciesScientific == "Katsuwonus pelamis", `:=` (SpeciesName = "Skipjack tuna", SpeciesCode = "SKJ")]
FISHBASE_LW[SpeciesScientific == "Thunnus albacares",  `:=` (SpeciesName = "Yellowfin tuna", SpeciesCode = "YFT")]

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

# Harmonize sex
FISHBASE_LW[Sex == "unsexed", Sex := "Unsexed"]
FISHBASE_LW[Sex == "mixed", Sex := "Mixed"]

# Delete sources already collated from literature
FISHBASE_LW = FISHBASE_LW[AuthorStd != "Andrade, H.A.; Campos, R.O."]
FISHBASE_LW = FISHBASE_LW[AuthorStd != "Uchiyama, J.H."]

# Delete records without info on n or with less than 100 fish
FISHBASE_LW = FISHBASE_LW[!is.na(Number) & Number>=20]

# a parameters for FL -> RW relationship
FISHBASE_PARAMS = FISHBASE_LW[LengthType == "FL", .(Ocean, SpeciesCode, SpeciesName, SpeciesScientific, Sex, Source = "LF", Target = "RD", SampleSize = Number, ForkLengthMin = LengthMin, ForkLengthMax = LengthMax, a = a/1000, b, Reference = AuthorStd)][, Origin := "FISHBASE"]

# TUNA RFMOS ####

# https://www.iccat.int/Documents/SCRS/Manual/Appendices/Appendix%204%20III%20Trop.pdf
TRFMO_PARAMS = data.table(read.xlsx("../inputs/data/TRFMO_PARAMETERS.xlsx", sheet = "OFFICIAL"))[, Origin := "REFERENCE"]
OTHER_PARAMS = data.table(read.xlsx("../inputs/data/TRFMO_PARAMETERS.xlsx", sheet = "OTHERS"))[, Origin := "OTHERS"]

# COMBINE LITERATURE PARAMETERS ####

FL_RD_PARAMS_REVIEW = rbindlist(list(FISHBASE_PARAMS, TRFMO_PARAMS, OTHER_PARAMS), use.names = TRUE)

print("Length-weight parameters for tropical tuna reviewed!")
