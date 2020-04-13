#!/usr/bin/env Rscript
influence2.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/influence2.txt"
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
suppressPackageStartupMessages(library(VGAM))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(MASS))

main <- function(argv) {

  studentize(influence2.txt())
  return(0)
}

studentize <- function(path) {
  data <- read.table(path,
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  plot(data$x, data$y, xlab = "X", ylab = "Y",
  main = "Scatter plot of y versus x",
  sub = path,
  xlim = c(min(data$x), max(data$x)),
  ylim = c(min(data$y), max(data$y))
  )
  lm <- lm(y ~ x, data)
  data %<>%
    mutate(studres = studres(lm))
  print(head(data))
  print(skimr::skim(data))
  data2 <- data %>%
    filter(abs(studres) > 2)
  print(data2)
  points(data2$x, data2$y, col = "red", pch = 4)
  abline(v = mean(data$x))
  boxplot(data$studres,
  main = path,
  xlab = "Studentized residuals",
  col = "pink",
  border = "blue",
  horizontal = TRUE,
  notch = FALSE)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
