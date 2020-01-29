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
  print(letters[1:4] %>%
    reduce(paste2))
  print(letters[1:4] %>%
    reduce2(c("-", ".", "-"), paste2))

  samples <- purrr::rerun(2, sample(10, 5))
  print(samples)
  print(purrr::reduce(samples, union))
  print(purrr::reduce(samples, intersect))

  x <- list(c(0, 1), c(2, 3), c(4, 5))
  print(x %>%
    reduce(c))
  print(x %>%
    reduce(c, .dir = "backward"))
  # Equivalent to:
  print(x %>%
    rev() %>%
    reduce(c))
  # }
  reduce(letters[1:4],
    .dir = "backward",
    function(x, y) {
      message("x is ", x)
      message("y is ", y)
      message("")
      paste0(x, y)
    }
  )
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
