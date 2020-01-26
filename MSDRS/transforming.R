#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(ggplot2)
suppressMessages(library(gridExtra))
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

  andrew_tracks %>%
    gather(measure, value, -datetime) %>%
    ggplot(aes(x = datetime, y = value)) +
    geom_point() + geom_line() +
    facet_wrap(~measure, ncol = 1, scales = "free_y")
  ggplot2::ggsave("andrew_tracks.pdf")

  print(andrew_tracks %>%
    select(datetime) %>%
    mutate(
      year = year(datetime),
      month = months(datetime),
      weekday = weekdays(datetime),
      yday = yday(datetime),
      hour = hour(datetime)
    ) %>%
    slice(1:3))

  check_tracks <- ext_tracks %>%
    select(month, day, hour, year, max_wind) %>%
    unite(datetime, year, month, day, hour) %>%
    mutate(
      datetime = ymd_h(datetime),
      weekday = weekdays(datetime),
      weekday = factor(weekday,
        levels = c(
          "Sunday", "Monday",
          "Tuesday", "Wednesday",
          "Thursday", "Friday",
          "Saturday"
        )
      ),
      month = months(datetime),
      month = factor(month, levels = c(
        "April", "May", "June",
        "July", "August", "September",
        "October", "November",
        "December", "January"
      ))
    )

  check_weekdays <- check_tracks %>%
    group_by(weekday) %>%
    summarize(ave_max_wind = mean(max_wind)) %>%
    rename(grouping = weekday)

  check_months <- check_tracks %>%
    group_by(month) %>%
    summarize(ave_max_wind = mean(max_wind)) %>%
    rename(grouping = month)

  a <- ggplot(check_weekdays, aes(x = grouping, y = ave_max_wind)) +
    geom_bar(stat = "identity") + xlab("")
  b <- ggplot2::`%+%`(a, check_months)
  gridExtra::grid.arrange(a, b, ncol = 1)
  c <- gridExtra::arrangeGrob(grobs = list(a, b), ncol = 1)
  ggplot2::ggsave("grid.pdf", c)


  andrew_tracks <- ext_tracks %>%
    filter(storm_name == "ANDREW") %>%
    slice(23:47) %>%
    select(year, month, day, hour, latitude, longitude) %>%
    unite(datetime, year, month, day, hour) %>%
    mutate(
      datetime = ymd_h(datetime),
      date = format(datetime, "%b %d")
    )
  print(andrew_tracks)
  if (library(ggmap, logical.return = TRUE)) {
    miami <- ggmap::get_map("miami", zoom = 5)
    ggmap(miami) +
      geom_path(
        data = andrew_tracks, aes(x = -longitude, y = latitude),
        color = "gray", size = 1.1
      ) +
      geom_point(
        data = andrew_tracks,
        aes(x = -longitude, y = latitude, color = date),
        size = 2
      )
    ggplot2::ggsave("miamimap.pdf")
  } else {
    cat("package ggmap not found.\n")
  }
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
