#!/usr/bin/env Rscript
library(devtools)
library(parallel)
library(rprojroot)

create_package <- function(folder) {
  path <- file.path(".", folder)
  if (dir.exists(path)) {
    stop("Folder", path, "already exists.", call = TRUE)
  }
  setwd("..")
  devtools::create(path)
}

main <- function(argv) {
  print(sessionInfo())
  parallel::mclapply(argv, create_package)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
