#!/usr/bin/env Rscript
library(dplyr)
library(httr)
library(magrittr)
library(readr)

# nolint start
main <- function(argv) {
  meso_url <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py/"
  denver <- httr::GET(url = meso_url,
                      query = list(station = "DEN",
                                   data = "sped",
                                   year1 = "2016",
                                   month1 = "6",
                                   day1 = "1",
                                   year2 = "2016",
                                   month2 = "6",
                                   day2 = "30",
                                   tz = "America/Denver",
                                   format = "comma")) %>%
    httr::content() %>%
    readr::read_csv(skip = 5, na = "M")

  print(denver %>% dplyr::slice(1:3))
  return(0)
}
# nolint end

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
