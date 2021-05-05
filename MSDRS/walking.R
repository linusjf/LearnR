#!/usr/bin/env Rscript
library(purrr)

main <- function(argv) {
  print(walk(c("Friends, Romans, countrymen,", "lend me your ears;", "I come to bury Caesar,", 
    "not to praise him."), message))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
