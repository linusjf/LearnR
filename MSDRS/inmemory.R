#!/usr/bin/env Rscript
library(data.table)
suppressMessages(library(dplyr))

main <- function(argv) {
  brazil_zika <- data.table::fread("COES_Microcephaly-2016-06-25.csv")
  print(head(brazil_zika, 2))
  print(data.table::fread("COES_Microcephaly-2016-06-25.csv", select = c(
    "location",
    "value", "unit"
  )) %>%
    dplyr::slice(1:3))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
