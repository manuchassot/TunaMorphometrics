
print("Read morphometric data...")

# Main data set ####

SAMPLES_WITH_ENVIRONMENT = data.table(read.xlsx("../inputs/data/Alltuna_Final.xlsx"))[ocean_code %in% c("AO", "IO")]

# Format sampling date
SAMPLES_WITH_ENVIRONMENT[, organism_sampling_date := convertToDate(organism_sampling_date, origin = "1900-01-01")] 

# Format landing date
SAMPLES_WITH_ENVIRONMENT[, landing_date := convertToDate(landing_date, origin = "1900-01-01")]

# Format capture date
SAMPLES_WITH_ENVIRONMENT[, capture_date_avg := convertToDate(capture_date_avg, origin = "1900-01-01")]

# IOTTP data set ####
# IO_IOTTP     = fread("../inputs/data/data_iottp_fl_wt_2009_2015.csv")
# IO_IOTTP[, ocean_code := "IO"]
# names(IO_IOTTP) = c("project", "sampling_year", "species_code_fao", "sex", "fork_length", "whole_fish_weight", "ocean_code")
# IO_IOTTP[sex == "", sex := NA]

print("Morphometric data read")
