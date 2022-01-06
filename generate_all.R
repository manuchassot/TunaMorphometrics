# Source the R codes
setwd("initialization")
source("00_CORE.R")
setwd("..")

TITLE = "Tuna_Morphometrics"

options(scipen = 100)

# DOCX
render("rmd/00_DOCX.Rmd", 
       output_dir    = "outputs/", 
       output_file   = paste0(TITLE, ".docx")
)


