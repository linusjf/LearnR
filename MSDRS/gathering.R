#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(tidyr)

main <- function(argv) {
  data("VADeaths")
  print(head(VADeaths))

  # Move age from row names into a column
  va_deaths <- VADeaths %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(Age = row.names(VADeaths))
  print(va_deaths)
  # Gather everything but age to tidy data
  print(va_deaths %>%
    tidyr::gather(key = "key", value = "death_rate", -Age))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
