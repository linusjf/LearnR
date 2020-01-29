#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(readr)

check_for_logfile <- function(date) {
  year <- substr(date, 1, 4)
  src <- sprintf(
    "http://cran-logs.rstudio.com/%s/%s.csv.gz",
    year, date
  )
  dest <- file.path(".", basename(src))
  if (!file.exists(dest)) {
    val <- download.file(src, dest, quiet = TRUE)
    if (!val) {
      stop("unable to download file ", src)
    }
  }
  return(dest)
}

## pkgname: package name (character)
## date: YYYY-MM-DD format (character)
num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()
  dest <- check_for_logfile(date)
  count <- read_csv(dest, col_types = "ccicccccci", progress = FALSE) %>%
    filter(package == pkgname) %>%
    nrow()
  return(count)
}

check_pkg_deps <- function() {
  if (!require(readr)) {
    message("installing the 'readr' package")
    install.packages("readr")
  }
  if (!require(dplyr)) {
    stop("the 'dplyr' package needs to be installed first")
  }
}

main <- function(argv) {
  print(num_download("filehash", "2016-07-20"))
  print(num_download("Rcpp", "2016-07-19"))
  print(num_download("Rcpp"))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
