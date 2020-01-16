#!/usr/bin/env Rscript
library(lintr)
library(styler)

main <- function(argv) {
  for (folder in list.dirs(path = ".", recursive = FALSE)) {
    if (folder == "./.git") 
      next
    print(folder)
    print(lintr::lint_dir(folder))
  }
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
