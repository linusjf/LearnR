#!/usr/bin/env Rscript
library(skimr)
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
  print(head(data))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
