#!/usr/bin/env Rscript
# nolint start
google_stock.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/google_stock.txt"
  )
}
# nolint end

lib_path <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}

library(skimr)
source(lib_path())

main <- function(argv) {
  pdf(paper = "USr")
  data <- read.table(google_stock.txt(),
    header = TRUE, as.is = TRUE
  )
  print(str(data$date))
  print(head(data))
  data$date <- as.POSIXct(data$date, format = "%m/%d/%Y")
  print(skimr::skim(data))

  attach(data)
  plot(
    x = date, y = price, col = "blue",
    pch = 15, type = "b",
    main = "Time series plot of price versus date",
    xlab = "Date", ylab = "Price"
  )
  detach(data)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
