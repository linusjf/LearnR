#!/usr/bin/env Rscript
library(lintr)
library(parallel)

process_folder <- function(folder) {
  cat(folder, "\n")
  return(lintr::lint_dir(folder, parse_settings = TRUE))
}

main <- function(argv) {
  print(sessionInfo())
  folders <- list.dirs(path = ".", recursive = FALSE)
  folders <- folders[folders != "./.git"]
  violations_list <- parallel::mclapply(folders, process_folder)
  violations <- do.call(c, violations_list)

  # process root folder . as well
  for (file in list.files(
    pattern = "\\.[Rr]{1}(profile)?$",
    all.files = TRUE
  )) {
    cat(file, "\n")
    violations <- append(violations, lintr::lint(file))
  }
  if (length(violations) > 0) {
    print(violations)
  }
  return(length(violations))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
