#!/usr/bin/env Rscript
library(microbenchmark)

beera <- function(expr) {
  tryCatch(expr, error = function(e) {
    message("An error occurred:\n", e)
  }, warning = function(w) {
    message("A warning occured:\n", w)
  }, finally = {
    message("Finally done!")
  })
}

is_even_check <- function(n) {
  return(is.numeric(n) && n%%2 == 0)
}

is_even_error <- function(n) {
  tryCatch(n%%2 == 0, error = function(e) {
    FALSE
  })
}

main <- function(args) {
  print(beera({
    2 + 2
  }))
  print(beera({
    "two" + 2
  }))
  print(beera({
    as.numeric(c(1, "two", 3))
  }))
  print(microbenchmark(sapply(letters, is_even_check), units = "ns"))
  print(microbenchmark(sapply(letters, is_even_error), units = "ns"))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
