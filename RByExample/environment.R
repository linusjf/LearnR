#!/usr/bin/env Rscript
f <- function(f_x) {
  g <- function(g_x) {
    print("Inside g")
    print(environment())
    print(ls())
  }
  g(5)
  print("Inside f")
  print(environment())
  print(ls())
}

main <- function(argv) {
  print("Inside main")
  print(argv)
  print(environment())
  f(10)
  return(0)
}

print(environment())
if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
