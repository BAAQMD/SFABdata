suppressPackageStartupMessages({
  library(dplyr)     # install.packages("dplyr")
  library(tidyr)     # install.packages("tidyr")
  library(httr)      # install.packages("httr")
  library(lubridate) # install.packages("lubridate")
  library(stringr)   # install.packages("stringr")
  library(ggplot2)   # install.packages("ggplot2")
  library(digest)
})

TODAY <- as.Date(Sys.time())

LST <- "America/Los_Angeles"

QA_FLAGS <- c(
  "Valid" = "0",
  "Valid, flagged by data supplier" = "1",
  "Uncertain, flagged by QA routine" = "2",
  "Invalid, flagged by QA routine" = "3",
  "Invalid, flagged by data supplier" = "4",
  "Invalid, flagged manually" = "5")

PARAMETERS <- c(
  "Ozone" = "OZONE",
  "PM2.5" = "PM25HR",
  "Black Carbon" = "BC")

UNITS <- c(
  "ug/m3" = "001",
  "ppm" = "007",
  "ppb" = "008")

PARAM_UNITS <- c(
  "OZONE" = "008",
  "PM25HR" = "001",
  "BC" = "001")

cached <- function(file, expr, cache_dir = "cache", compress = "xz", verbose = TRUE) {
  file <- normalizePath(file.path(cache_dir, file), mustWork = FALSE)
  if (!file.exists(file)) {
    if (verbose) message("[cached] miss:", file)
    if (!file.exists(dn <- dirname(file))) dir.create(dn, recursive = TRUE)
    saveRDS(obj <- force(expr), file = file, compress = compress)
  } else {
    if (verbose) message("[cached] hit:", file)
    obj <- readRDS(file)
  }
  return(obj)
}
