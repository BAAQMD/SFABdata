suppressPackageStartupMessages({
  library(dplyr)  # install.packages("dplyr")
  library(tidyr)  # install.packages("tidyr", type="source")
  library(httr)   # install_github("hadley/httr")
  library(rvest)  # install_github("hadley/rvest")
})

QA_FLAGS <- c(
  "Valid observation" = "0",
  "The data supplier marked the observation as suspect - but it is still valid" = "1",
  "The automated qa routine judged the observation questionable and invalid" = "2",
  "The automated qa routine judged the observation invalid" = "3",
  "The data supplier flagged the observation invalid" = "4",
  "The observation was flagged invalid manually" = "5")

PARAMETERS <- c(
  "Ozone" = "OZONE",
  "PM2.5" = "PM25HR")

UNITS <- c(
  "ppm" = "007",
  "ppb" = "008")
