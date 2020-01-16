#!/usr/bin/env Rscript
library(formatR)

main <- function(argv) {
    print(sessionInfo())
    options("formatR.indent=2")
    path = dir(path = ".", pattern = "^[a-z].+[.]R$")
    tidy_file(list.files(path = ".", pattern = "^[a-z].*[.]R$"))
    path = dir(path = ".", pattern = "^[A-Z].+$")
    tidy_dir(path, recursive = TRUE)
    return(0)
}

if (identical(environment(), globalenv())) {
    quit(status = main(commandArgs(trailingOnly = TRUE)))
}
