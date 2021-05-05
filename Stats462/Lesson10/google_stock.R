#!/usr/bin/env Rscript
# nolint start
google_stock.txt <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/google_stock.txt")
}
# nolint end

lib_path <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Lib/libfunc.R")
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(Hmisc))

main <- function(argv) {
  pdf(paper = "USr")
  data <- read.table(google_stock.txt(), header = TRUE, as.is = TRUE)
  print(str(data$date))
  print(head(data))
  data$date <- as.POSIXct(data$date, format = "%m/%d/%Y")
  print(skimr::skim(data))

  attach(data)
  plot(x = date, y = price, col = "blue", pch = 15, type = "b", main = "Time series plot of price", 
    xlab = "Date", ylab = "Price", xaxt = "n")
  at <- date[c(1, 16, 31, 46, 61, 76, 91, 105)]
  labels <- format(at, format = "%m/%d/%Y")
  axis(1, at = at, labels = labels, tick = TRUE)
  Pacf(price)
  detach(data)

  data %<>%
    mutate(lag_1_price = Lag(price, 1))
  attach(data)
  plot(y = price, x = lag_1_price, col = "blue", pch = 15, main = "Scatter plot of Price versus Lagged Price", 
    ylab = "Price", xlab = "Lag 1 Price")
  detach(data)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
