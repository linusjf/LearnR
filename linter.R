#!/usr/bin/env Rscript
library(lintr)

main <- function(argv) {
  print(sessionInfo())
  exit_code <- 0
  for (folder in list.dirs(path = ".", recursive = FALSE)) {
    if (folder == "./.git") {
      next
    }
    cat(folder, "\n")
    violations <- lint_dir(folder, parse_settings = TRUE)
    exit_code <- exit_code + length(violations)
    print(violations)
  }
  return(exit_code)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
