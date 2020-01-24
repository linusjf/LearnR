#!/usr/bin/env Rscript
library(lintr)

main <- function(argv) {
  print(sessionInfo())
  exit_code <- 0
  for (folder in argv) {
    if (folder == "./.git") {
      next
    }
    cat(folder, "\n")
    if (folder == ".") {
      for (file in list.files(pattern = "\\.[Rr]$")) {
        violations <- lintr::lint(file)
        exit_code <- exit_code + length(violations)
        print(violations)
      }
      next
    }
    violations <- lintr::lint_dir(folder, parse_settings = TRUE)
    exit_code <- exit_code + length(violations)
    print(violations)
  }
  return(exit_code)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
