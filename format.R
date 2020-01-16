#!/usr/bin/env Rscript
library(formatR)

main <- function(argv) {
path = dir(".")
tidy_dir(path, recursive = TRUE)
}

if (identical (environment (), globalenv ()))
  quit (status = main(commandArgs(trailingOnly = TRUE)))
