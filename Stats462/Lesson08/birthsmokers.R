#!/usr/bin/env Rscript
library(skimr)
library(scatterplot3d)
suppressPackageStartupMessages(
                               library(dplyr))
library(magrittr)
source("../Lib/libfunc.R")

main <- function(argv) {
  data <- read.table("../Data/birthsmokers.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  scatterplot_matrix(data, "Birth Smokers Scatterplot Matrix")
  data %<>%
    mutate(bin_smokers = ifelse(data$Smoke == "yes", 1, 0))
  data$Smoke <- NULL
  print(head(data))
  # 3D scatter plot
  with(data, {
  s3d <- scatterplot3d(data,
    type = "p", color = "blue",
    angle = 55, pch = 16
  )
  }
  )
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
