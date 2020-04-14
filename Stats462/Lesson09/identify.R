#!/usr/bin/env Rscript
influence2.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/influence2.txt"
  )
}

influence3.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/influence3.txt"
  )
}

influence4.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/influence4.txt"
  )
}

lib_path <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}

library(skimr)
source(lib_path())
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- c(influence2.txt(),
            influence3.txt(),
            influence4.txt()
  )

  lapply(data, dffits)
  return(0)
}

dffits <- function(path) {
  data <- read.table(path,
    header = TRUE
  )
  lm <- lm(y ~ x, data)
  print(head(data))
  print(skimr::skim(data))
  plot(data$x, data$y, xlab = "X", ylab = "Y",
  main = "Scatter plot of y versus x")
  data2 <- tail(data, 1)
  points(data2$x, data2$y, col = "red")
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
