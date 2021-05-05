#!/usr/bin/env Rscript
library(purrr)

main <- function(argv) {
  print(map_chr(c(5, 4, 3, 2, 1), function(x) {
    c("one", "two", "three", "four", "five")[x]
  }))

  print(map_lgl(c(1, 2, 3, 4, 5), function(x) {
    x > 3
  }))

  print(map_if(1:5, function(x) {
    x%%2 == 0
  }, function(y) {
    y^2
  }) %>%
    unlist())

  print(map_at(seq(100, 500, 100), c(1, 3, 5), function(x) {
    x - 10
  }) %>%
    unlist())

  print(purrr::map2_chr(letters, 1:26, paste))

  print(pmap_chr(list(list(1, 2, 3), list("one", "two", "three"), list("uno", "dos", 
    "tres")), paste))

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
