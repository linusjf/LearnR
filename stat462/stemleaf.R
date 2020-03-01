#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("houseprice.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  data <- data[["Price"]]
  stem(data)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
