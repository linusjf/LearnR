#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))

main <- function(argv) {
  data <- read.table("../Data/Physical.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
