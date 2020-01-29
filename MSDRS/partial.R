#!/usr/bin/env Rscript
library(purrr)

mult_three_n <- function(x, y, z) {
  x * y * z
}

main <- function(argv) {
  mult_by_15 <- purrr::partial(mult_three_n,
    x = 3,
    y = 5
  )

  print(mult_by_15(z = 4))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
