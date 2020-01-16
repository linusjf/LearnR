#!/usr/bin/env Rscript
library(styler)

main <- function(argv) {
  print(sessionInfo())
  exit_code <- 0
  for (folder in list.dirs(path = ".", recursive = FALSE)) {
    if (folder == "./.git")
      next
    cat(folder, "\n")
    style_dir(folder, recursive = TRUE)
  }
  return(exit_code)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
