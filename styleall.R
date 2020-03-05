#!/usr/bin/env Rscript
library(styler)
library(parallel)

process_folder <- function(folder) {
  cat(folder, "\n")
  res <- as.data.frame(styler::style_dir(folder, recursive = FALSE))
  return(nrow(subset(res, res$changed == TRUE)))
}

main <- function(argv) {
  print(sessionInfo())
  folders <- list.dirs(path = ".", recursive = TRUE)
  folders <- folders[
    grep("^.*[.]git.*$",
      folders,
      invert = TRUE
    )
  ]
  print(folders)
  exit_codes <- parallel::mclapply(folders, process_folder)
  return(sum(unlist(exit_codes)))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
