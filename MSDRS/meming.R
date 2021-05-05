#!/usr/bin/env Rscript
library(magrittr)
library(pryr)

main <- function(argv) {
  print(pryr::mem_used())
  print(ls())
  print(pryr::object_size(argv))
  print(sapply(ls(), function(x) {
    object.size(get(x))
  }) %>%
    sort() %>%
    tail(5))
  print(pryr::mem_used())
  print(pryr::mem_change(rm(argv)))
  print(pryr::mem_used())
  print(pryr::object_size(integer(0)))
  print(pryr::object_size(integer(1000)))
  print(pryr::object_size(numeric(1000)))
  print(str(.Machine))
  print(gc())
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
