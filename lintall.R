#!/usr/bin/env Rscript
library(lintr)
library(parallel)
library(rex)

process_folder <- function(folder) {
  cat(folder, "\n")
  pattern <- rex::regex("\\.[Rr]{1}(profile)?$")
  return(lintr::lint_dir(folder, parse_settings = TRUE, pattern = pattern))
}

main <- function(argv) {
  print(sessionInfo())
  folders <- list.dirs(path = ".", recursive = TRUE)
  folders <- folders[grep("^.*[.]git.*$", folders, invert = TRUE)]
  print(folders)
  folders <- folders[folders != "./.git"]
  violations_list <- parallel::mclapply(folders, process_folder)
  violations <- do.call(c, violations_list)

  if (length(violations) > 0) {
    print(violations)
  }
  return(length(violations))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
