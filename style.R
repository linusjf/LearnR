#!/usr/bin/env Rscript
library(styler)
library(parallel)

process_folder <- function(folder) {
  exit_code <- 0

  if (folder == "./.git" || folder == ".git") {
    return(exit_code)
  }
  cat(folder, "\n")
  res <- as.data.frame(styler::style_dir(folder, recursive = FALSE))
  return(nrow(subset(res, res$changed == TRUE)))
}

main <- function(argv) {
  print(sessionInfo())
  exit_codes <- parallel::mclapply(argv, process_folder)
  return(sum(unlist(exit_codes)))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
