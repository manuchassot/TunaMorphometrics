
print("Initialisation of global tuna catch...")

# Extract the data from the GTA database
NC_YEAR_RFMO_GEAR_GROUP_SPECIES = as.data.table(dbGetQuery(con_GTA,
"SELECT DISTINCT year AS \"YEAR\", source_authority AS \"RFMO\", gear_group AS \"GEAR_GROUP\", species AS \"SPECIES\", sum(value) AS \"CATCH\"
FROM fact_tables.global_nominal_catch_firms_level0
WHERE species IN ('ALB', 'BET', 'SKJ', 'YFT')
AND year>1949
GROUP BY year, source_authority, gear_group, species
ORDER BY year, source_authority, gear_group, species;"
))

# Add missing catch for IATTC in 2019
# https://www.iattc.org/PublicDomainData/CatchByFlagGear.zip
NC_IATTC_2019 = fread("../inputs/data/CatchFlagGear1918-2019.csv")[AnoYear == 2019]

# Format gear groups
NC_IATTC_2019[ArteGear %in% c('OTR', 'GN', 'LTL', 'RG'), ArteGear := "OTHER"]
NC_IATTC_2019[ArteGear == 'NK',                          ArteGear := "UNK"]

NC_YEAR_RFMO_GEAR_GROUP_SPECIES_IATTC_2019 = NC_IATTC_2019[EspeciesSpecies %in% c("ALB", "BET", "SKJ", "YFT"), .(RFMO = "IATTC", CATCH = sum(t, na.rm = T)), keyby = .(YEAR = AnoYear, GEAR_GROUP = ArteGear, SPECIES = EspeciesSpecies)]

NC_YEAR_RFMO_GEAR_GROUP_SPECIES = rbindlist(list(NC_YEAR_RFMO_GEAR_GROUP_SPECIES, NC_YEAR_RFMO_GEAR_GROUP_SPECIES_IATTC_2019), use.names = TRUE)

# Composition by species in recent years

NC_YEAR_SPECIES = NC_YEAR_RFMO_GEAR_GROUP_SPECIES[, .(CATCH = sum(CATCH)), keyby = .(YEAR, SPECIES)]

NC_YEAR_SPECIES_RECENT = NC_YEAR_SPECIES[YEAR %in% 2015:2019, .(CATCH = round(sum(CATCH/5))), keyby = .(SPECIES)]
NC_YEAR_SPECIES_RECENT[, TOTAL_CATCH   := sum(CATCH)]
NC_YEAR_SPECIES_RECENT[, PERCENT_CATCH := round(CATCH/TOTAL_CATCH*100, 1)]

print("Global tuna catch initialized!")
