#!/usr/bin/env Rscript
earthquakes.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/earthquakes.txt")
}

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(Hmisc))

main <- function(argv) {
  data <- read.table(earthquakes.txt(), header = TRUE, as.is = TRUE)
  print(head(data))
  print(skimr::skim(data))

  attach(data)
  plot(
    x = Year, y = Quakes, col = "blue", pch = 15, type = "b", main = "Time series plot of quakes",
    xlab = "Year", ylab = "Quakes"
  )
  Pacf(Quakes)
  detach(data)

  data %<>%
    mutate(lag1Quakes = Lag(Quakes, 1)) %>%
    mutate(lag2Quakes = Lag(Quakes, 2)) %>%
    mutate(lag3Quakes = Lag(Quakes, 3))

  model <- lm(Quakes ~ lag1Quakes + lag2Quakes + lag3Quakes, data)

  print(model_coeffs(model))
  print(model_fit_stats(model))
  print(model_equation(model, digits = 4))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
