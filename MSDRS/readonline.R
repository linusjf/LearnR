#!/usr/bin/env Rscript
library(readr)
library(dplyr)

main <- function(argv) {
  ext_tracks_file <- paste0(
    "http://rammb.cira.colostate.edu/research/",
    "tropical_cyclones/tc_extended_best_track_dataset/",
    "data/ebtrk_atlc_1988_2015.txt"
  )

  # Create a vector of the width of each column
  ext_tracks_widths <- c(
    7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
    4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1
  )

  # Create a vector of column names, based on the online documentation for this
  # data
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

  # Read the file in from its url
  ext_tracks <- read_fwf(ext_tracks_file,
    fwf_widths(ext_tracks_widths, ext_tracks_colnames),
    na = "-99"
  )
  ext_tracks[1:3, 1:9]

print(ext_tracks %>%
  filter(storm_name == "KATRINA") %>%
  select(month, day, hour, max_wind, min_pressure, rad_max_wind) %>%
  sample_n(4))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
