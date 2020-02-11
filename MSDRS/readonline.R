#!/usr/bin/env Rscript
suppressMessages(library(readr))
suppressMessages(library(dplyr))
library(ggplot2)

knots_to_mph <- function(knots) {
  mph <- 1.152 * knots
  return(mph)
}

main <- function(argv) {
  ext_tracks_file <<- paste0(
    "http://rammb.cira.colostate.edu/research/",
    "tropical_cyclones/tc_extended_best_track_dataset/",
    "data/ebtrk_atlc_1988_2015.txt"
  )

  # Create a vector of the width of each column
  ext_tracks_widths <<- c(
    7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
    4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1
  )

  # Create a vector of column names, based on the online documentation for this
  # data
  ext_tracks_colnames <<- c(
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
  ext_tracks <<- readr::read_fwf(ext_tracks_file,
    fwf_widths(ext_tracks_widths, ext_tracks_colnames),
    na = "-99"
  )
  sampling()
  summarizing()
  grouping()
  selecting()
  filtering()
  headings()
  wind()
  andrewcat5()
  zika()
  graphit()
  return(0)
}

wind <- function() {
  print(ext_tracks %>%
    group_by(storm_name, year) %>%
    summarize(worst_wind = max(max_wind)) %>%
    filter(worst_wind >= 160))
}

andrewcat5 <- function() {
  print(ext_tracks %>%
    select(storm_name, month, day, hour, latitude, longitude, max_wind) %>%
    filter(storm_name == "ANDREW" & max_wind >= 137))
}

headings <- function() {
  print(ext_tracks %>%
    select(storm_name, hour, max_wind) %>%
    head(9))
  print(ext_tracks %>%
    select(storm_name, hour, max_wind) %>%
    filter(hour == "00") %>%
    head(3))
}

sampling <- function() {
  print(ext_tracks[1:3, 1:9])

  print(ext_tracks %>%
    filter(storm_name == "KATRINA") %>%
    select(month, day, hour, max_wind, min_pressure, rad_max_wind) %>%
    sample_n(4))
}


summarizing <- function() {
  print(ext_tracks %>%
    summarize(
      n_obs = n(),
      worst_wind = knots_to_mph(max(max_wind)),
      worst_pressure = min(min_pressure)
    ))
}

grouping <- function() {
  print(ext_tracks %>%
    group_by(storm_name, year) %>%
    head())

  print(ext_tracks %>%
    group_by(storm_name, year) %>%
    summarize(
      n_obs = n(),
      worst_wind = max(max_wind),
      worst_pressure = min(min_pressure)
    ))
}

selecting <- function() {
  print(ext_tracks %>%
    dplyr::select(
      storm_name, month, day, hour, year, latitude, longitude,
      max_wind
    ))

  print(dplyr::select(ext_tracks, dplyr::ends_with("ne")))

  print(dplyr::select(ext_tracks, dplyr::contains("34")))

  print(dplyr::select(ext_tracks, dplyr::matches("_[0-9][0-9]_")))
}

filtering <- function() {
  print(filter(ext_tracks, ext_tracks$storm_name == "KATRINA"))

  print(filter(ext_tracks, ext_tracks$min_pressure != 0))

  print(filter(ext_tracks, ext_tracks$latitude > 25))

  print(filter(ext_tracks, ext_tracks$max_wind >= 160))

  print(filter(ext_tracks, ext_tracks$min_pressure < 900))

  print(filter(ext_tracks, ext_tracks$distance_to_land <= 0))

  print(filter(ext_tracks, ext_tracks$storm_name %in% c("KATRINA", "ANDREW")))

  print(filter(ext_tracks, is.na(ext_tracks$radius_34_ne)))
}

zika <- function() {
  zika_file <<- paste0(
    "https://raw.githubusercontent.com/cdcepi/zika/master/",
    "Brazil/COES_Microcephaly/data/COES_Microcephaly-2016-06-25.csv"
  )
  zika_brazil <<- readr::read_csv(zika_file)

  print(zika_brazil %>%
    select(location, value, unit))
}

graphit <- function() {
  ext_tracks %>%
    group_by(storm_name) %>%
    summarize(worst_wind = max(max_wind)) %>%
    ggplot2::ggplot(aes(x = worst_wind)) +
    geom_histogram(bins = 30)
  ggplot2::ggsave("ext_tracks.pdf")
}

zika_file <- NULL
zika_brazil <- NULL
ext_tracks_colnames <- NULL
ext_tracks <- NULL
if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
