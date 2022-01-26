# Install/load libraries required for analysis
# Install/load pacman
if(!require(pacman)){
  install.packages("pacman")
  suppressPackageStartupMessages(library(pacman,quietly = TRUE))
}

p_load(
  "tidyverse",
  "flextable",
  "scales",
  "openxlsx",
  "ggpubr",
  "gridExtra",
  "rmarkdown",
  "knitr",
  "bookdown",
  "officer",
  "dplyr",
  "ggsci",
  "RPostgreSQL",
  "data.table",
  "sf",
  "colorspace",
  "rfishbase",
  "patchwork",
  "rjags",
  "R2jags",
  "mcmcplots",
  "coda",
  "boa",
  "wellknown"
  )

