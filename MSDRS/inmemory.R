#!/usr/bin/env Rscript
library(data.table)

main <- function(argv) {
  brazil_zika <- data.table::fread("COES_Microcephaly-2016-06-25.csv")
  print(head(brazil_zika, 2))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
