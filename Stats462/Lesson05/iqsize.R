#!/usr/bin/env Rscript

source("libfunc.R")

main <- function(argv) {
  data <- read.table("../Data/iqsize.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  # nolint start
  scatterplot_matrix(data, "IQ Scatterplot Matrix")
  # nolint end
return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
