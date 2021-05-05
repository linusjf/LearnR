#!/usr/bin/env Rscript
houseprice() <- function() {
  library(rprojroot)
  paste0(find_root(has_file(".Rprofile")), "/Stats462/Data/houseprice.txt")
}
library(skimr)

main <- function(argv) {
  data <- read.table(houseprice(), header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  data <- data[["Price"]]
  print(t.test(data, conf.level = 0.95))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
