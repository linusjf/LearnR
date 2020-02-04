#!/usr/bin/env Rscript
library(profvis)
suppressMessages(library(dlnm))
data("chicagoNMMAPS")

f <- function() {
  highest_temp <- c()
  record_temp <- c()
  for (i in seq_len(nrow(datafr))) {
    highest_temp <- max(highest_temp, datafr$temp[i])
    record_temp[i] <- datafr$temp[i] >= threshold &
      datafr$temp[i] >= highest_temp
  }
  datafr <- cbind(datafr, record_temp)
}

main <- function(argv) {
  profile_data <- profvis::profvis(f(), prof_output = "prof_data.out")
  print(profile_data)
  tmp <- tempfile()
  print(tmp)
  Rprof(tmp, interval = 0.1)
  f()
  Rprof(NULL)
  writeLines(readLines(tmp))
  return(0)
}

datafr <- chicagoNMMAPS
threshold <- 27
if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
