#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(readr)

## pkgname: package name (character)
## date: YYYY-MM-DD format (character)
num_download <- function(pkgname, date) {
  ## Construct web URL
  year <- substr(date, 1, 4)
  src <- sprintf(
    "http://cran-logs.rstudio.com/%s/%s.csv.gz",
    year, date
  )

  ## Construct path for storing local file
  dest <- file.path(".", basename(src))

  ## Don't download if the file is already there!
  if (!file.exists(dest)) {
    download.file(src, dest, quiet = TRUE)
  }

  count <- readr::read_csv(dest,
    col_types = "ccicccccci", progress =
      FALSE
  ) %>%
    filter(package == pkgname) %>%
    nrow()
  return(count)
}

main <- function(argv) {
  print(num_download("filehash", "2016-07-20"))
  print(num_download("Rcpp", "2016-07-19"))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
