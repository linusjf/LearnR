#!/usr/bin/env Rscript
library(purrr)

main <- function(argv) {
  print(reduce(c(1, 3, 5, 7), function(x, y) {
    message("x is ", x)
    message("y is ", y)
    message("")
    x + y
  }))

  print(1:3 %>%
    reduce(`+`))
  print(1:10 %>%
    reduce(`*`))

  paste2 <- function(x, y, sep = ".") paste(x, y, sep = sep)
  letters[1:4] %>%
    reduce(paste2)
  letters[1:4] %>%
    reduce2(c("-", ".", "-"), paste2)

  samples <- purrr::rerun(2, sample(10, 5))
  print(samples)
  purrr::reduce(samples, union)
  purrr::reduce(samples, intersect)

  x <- list(c(0, 1), c(2, 3), c(4, 5))
  x %>%
    reduce(c)
  x %>%
    reduce_right(c)
  # Equivalent to:
  x %>%
    rev() %>%
    reduce(c)
  # }

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
