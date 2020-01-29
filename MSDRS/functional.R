#!/usr/bin/env Rscript

adder_maker <- function(n) {
  function(x) {
    n + x
  }
}

main <- function(argv) {
  add2 <- adder_maker(2)
  add3 <- adder_maker(3)

  print(add2(5))
  print(add3(5))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
