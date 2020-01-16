#!/usr/bin/env Rscript
library(lintr)

main <- function(argv) {
  exit_code <- 0
  for (folder in list.dirs(path = ".", recursive = FALSE)) {
    if (folder == "./.git") {
      next
    }
    cat(folder, "\n")
    violations <- lint_dir(folder)
    exit_code <- exit_code + length(violations)
    print(violations)
  }
  return(exit_code)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
