#!/usr/bin/env Rscript
library(readr)

main <- function(argv) {
  teams <- readr::read_csv("team_standings.csv",
    col_types = "ic"
  )
  print(teams)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
