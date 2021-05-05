#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
library(readr)

check_for_logfile <- function(date) {
  year <- substr(date, 1, 4)
  src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz", year, date)
  dest <- file.path(".", basename(src))
  if (!file.exists(dest)) {
    val <- download.file(src, dest, quiet = TRUE)
    if (!val) {
      stop("unable to download file ", src)
    }
  }
  return(dest)
}

## pkgname: package name (character) date: YYYY-MM-DD format (character) 'pkgname'
## can now be a character vector of names
num_download <- function(pkgname, date = "2016-07-20") {
  check_pkg_deps()

  ## Check arguments
  if (!is.character(pkgname)) {
    stop("'pkgname' should be character")
  }
  if (!is.character(date)) {
    stop("'date' should be character")
  }
  if (length(date) != 1) {
    stop("'date' should be length 1")
  }

  dest <- check_for_logfile(date)
  count_vector <- read_csv(dest, col_types = "ccicccccci", progress = FALSE) %>%
    filter(package %in% pkgname) %>%
    group_by(package) %>%
    summarize(n = n())
  return(count_vector)
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
  print(num_download(c("filehash", "weathermetrics")))
  print(num_download("filehash", c("2016-07-20", "2016-0-21")))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
