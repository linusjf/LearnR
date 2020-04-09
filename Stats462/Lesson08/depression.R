#!/usr/bin/env Rscript
depression.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/depression.txt"
  )
}

library(skimr)
library(scatterplot3d)
library(plot3D)
suppressPackageStartupMessages(
  library(dplyr)
)
library(magrittr)

main <- function(argv) {
  data <- read.table(depression.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  print(head(data))
  a <- subset(data, data$TRT == "A")
  b <- subset(data, data$TRT == "B")
  c <- subset(data, data$TRT == "C")
  plot(a$age, a$y,
    main = "y against age scatterplot",
    xlab = "Age", ylab = "Y",
    pch = 19, frame = FALSE,
    ylim = c(min(data$y), max(data$y)),
    xlim = c(min(data$age), max(data$age))
  )
  points(b$age, b$y,
    col = "blue",
    pch = 19
  )
  points(c$age, c$y,
    col = "green",
    pch = 19
  )
  box(which = "plot", lty = "solid")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
