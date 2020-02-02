#!/usr/bin/env Rscript
library(lintr)

main <- function(argv) {
  print(sessionInfo())
  exit_code <- 0
  folders <- list.dirs(path = ".", recursive = FALSE)
  folders <- folders[folders != "./.git"]
  for (folder in folders) {
    cat(folder, "\n")
    violations <- lintr::lint_dir(folder, parse_settings = TRUE)
    exit_code <- exit_code + length(violations)
    print(violations)
  }
  # process root folder . as well
  for (file in list.files(
    pattern = "\\.[Rr]{1}(profile)?$",
    all.files = TRUE
  )) {
    cat(file, "\n")
    violations <- lintr::lint(file)
    exit_code <- exit_code + length(violations)
    print(violations)
  }
  return(exit_code)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
