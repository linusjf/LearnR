#!/usr/bin/env Rscript
library(purrr)

main <- function(argv) {
  print(purrr::has_element(letters, "a"))
  print(purrr::has_element(letters, "A"))
  print(detect(20:40, function(x) {
  x > 22 && x %% 2 == 0
}))
  print(detect_index(20:40, function(x) {
  x > 22 && x %% 2 == 0
}))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
