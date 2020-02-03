#!/usr/bin/env Rscript
library(lintr)

process_folder <- function(folder) {
  violations <- as.list(c())
  if (folder == ".git" ||
      folder == "./.git")
    return(violations)
    cat(folder, "\n")
    if (folder == ".") {
      for (file in list.files(
        pattern = "\\.[Rr]{1}(profile)?$",
        all.files = TRUE
      )) {
        cat(file, "\n")
        violations <- append(violations, lintr::lint(file))
      }
      return(violations)
    }
    return(lintr::lint_dir(folder, parse_settings = TRUE))
}

main <- function(argv) {
  print(sessionInfo())
  violations_list <- lapply(argv, process_folder)
  violations <- do.call(c, violations_list)
  if (length(violations) > 0)
    print(violations)
  return(sum(unlist(violations)))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
