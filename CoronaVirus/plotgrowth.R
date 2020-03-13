#!/usr/bin/env Rscript
suppressMessages(library(readr))

main <- function(argv) {
  data <- read_csv("world_data.csv")
  infected <- data$confirmed
  dates <- data$date
 
old <- par(mfrow = c(1, 2))
plot(dates, infected, type ="b")
plot(dates, infected, log = "y")
abline(lm(log10(infected) ~ dates))
title("Confirmed Cases 2019-nCoV World", outer = TRUE, line = -2)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
