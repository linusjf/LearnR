#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
library(readr)
library(tidyr)

main <- function(argv) {
  print(lubridate::ymd("2006-03-12"))
  print(lubridate::ymd("'06 March 12"))
  print(lubridate::ymd_hm("06/3/12 6:30 pm"))
  ext_tracks_file <- "ebtrk_atlc_1988_2015.txt"
  ext_tracks_widths <- c(
    7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
    4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1
  )
  ext_tracks_colnames <- c(
    "storm_id", "storm_name", "month", "day",
    "hour", "year", "latitude", "longitude",
    "max_wind", "min_pressure", "rad_max_wind",
    "eye_diameter", "pressure_1", "pressure_2",
    paste("radius_34", c("ne", "se", "sw", "nw"),
      sep = "_"
    ),
    paste("radius_50", c("ne", "se", "sw", "nw"),
      sep = "_"
    ),
    paste("radius_64", c("ne", "se", "sw", "nw"),
      sep = "_"
    ),
    "storm_type", "distance_to_land", "final"
  )
  ext_tracks <- read_fwf(ext_tracks_file,
    fwf_widths(ext_tracks_widths, ext_tracks_colnames),
    na = "-99"
  )
  print(ext_tracks)

  andrew_tracks <- ext_tracks %>%
    filter(storm_name == "ANDREW" & year == "1992") %>%
    select(year, month, day, hour, max_wind, min_pressure) %>%
    unite(datetime, year, month, day, hour) %>%
    mutate(datetime = ymd_h(datetime))

  print(head(andrew_tracks, 3))
  print(class(andrew_tracks$datetime))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
