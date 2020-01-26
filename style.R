#!/usr/bin/env Rscript
library(styler)

main <- function(argv) {
  print(sessionInfo())
  exit_code <- 0
  for (folder in argv) {
    if (folder == "./.git") {
      next
    }
    cat(folder, "\n")
    res <- as.data.frame(styler::style_dir(folder, recursive = FALSE))
    exit_code <- exit_code + nrow(subset(res, res$changed == TRUE))
  }
  return(exit_code)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}