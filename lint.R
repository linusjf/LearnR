#!/usr/bin/env Rscript
library(lintr)

process_folder <- function(folder) {
  exit_code <- 0
  if (folder == ".git" ||
      folder == "./.git")
    return(exit_code)
    cat(folder, "\n")
    if (folder == ".") {
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
    violations <- lintr::lint_dir(folder, parse_settings = TRUE)
    exit_code <- exit_code + length(violations)
    print(violations)
    return(exit_code)
}

main <- function(argv) {
  print(sessionInfo())
  ret_codes <- lapply(argv, process_folder)
  return(sum(unlist(ret_codes)))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
