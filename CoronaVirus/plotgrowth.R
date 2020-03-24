#!/usr/bin/env Rscript
suppressMessages(library(readr))
suppressMessages(library(formattable))
suppressMessages(library(ihs))

main <- function(argv) {
  data <- readr::read_csv("world_data.csv")
  infected <- data$confirmed
  active_cases <- data$active.cases
  recovered <- data$recovered
  fatalities <- data$deaths
  growth <- formattable::percent(100 * data$confirmed.rt, 2)
  dates <- data$date

  par(mfrow = c(2, 2))
  plot(dates, infected, type = "b")
  plot(dates, infected, log = "y")
  abline(lm(log10(infected) ~ dates))
  plot(dates, growth, type = "b")
  plot(dates, growth, log = "y")
  lines(dates, growth)
  title("Confirmed Cases 2019-nCoV World", outer = TRUE, line = -2)

  par(mfrow = c(2, 1))
  plot(dates, infected, type = "h")
  plot(dates, infected, log = "y", type = "h")
  title("Confirmed Cases 2019-nCoV World", outer = TRUE, line = -2)
  
  par(mfrow = c(1, 1))
  plot(dates, infected, type = "b")
  points(dates, active_cases, col = "red")
  points(dates, recovered, col = "green")
  points(dates, fatalities, col = "blue")
  lines(dates, active_cases, col = "red")
  lines(dates, recovered, col = "green")
  lines(dates, fatalities, col = "blue")
  title("Confirmed Cases 2019-nCoV World", outer = TRUE, line = -2)

  data <- readr::read_csv("india_data.csv")
  infected <- data$confirmed
  active_cases <- data$active.cases
  recovered <- data$recovered
  fatalities <- data$deaths
  growth <- formattable::percent(100 * data$confirmed.rt, 2)
  dates <- data$date

  par(mfrow = c(2, 2))
  plot(dates, infected, type = "b")
  plot(dates, infected, log = "y")
  abline(lm(log10(infected) ~ dates))
  plot(dates, growth, type = "b")
  suppressWarnings(plot(dates, growth, log = "y"))
  lines(dates, growth)
  title("Confirmed Cases 2019-nCoV India", outer = TRUE, line = -2)

  par(mfrow = c(2, 1))
  plot(dates, infected, type = "h")
  plot(dates, infected, log = "y", type = "h")
  title("Confirmed Cases 2019-nCoV India", outer = TRUE, line = -2)
  
  par(mfrow = c(1, 1))
  plot(dates, infected, type = "b")
  points(dates, active_cases, col = "red")
  points(dates, recovered, col = "green")
  points(dates, fatalities, col = "blue")
  lines(dates, active_cases, col = "red")
  lines(dates, recovered, col = "green")
  lines(dates, fatalities, col = "blue")
  title("Confirmed Cases 2019-nCoV India", outer = TRUE, line = -2)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
