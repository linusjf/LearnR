#!/usr/bin/env Rscript
library(styler)

main <- function(argv) {
  print(sessionInfo())
  exit_code <- 0
  folders <- list.dirs(path = ".", recursive = FALSE)
  folders <- folders[folders != "./.git"]
  for (folder in folders) {
    cat(folder, "\n")
    res <- as.data.frame(styler::style_dir(folder, recursive = TRUE))
    exit_code <- exit_code + nrow(subset(res, res$changed == TRUE))
  }
  res <- as.data.frame(styler::style_dir(path = ".", recursive = FALSE))
  exit_code <- exit_code + nrow(subset(res, res$changed == TRUE))
  return(exit_code)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
