#!/usr/bin/env Rscript
library(readr)
suppressMessages(library(dplyr))

main <- function(argv) {
## Download data from RStudio (if we haven't already)
if (!file.exists("2016-07-20.csv.gz")) {
        download.file("http://cran-logs.rstudio.com/2016/2016-07-20.csv.gz",
                      "2016-07-20.csv.gz")
}
count <- readr::read_csv("2016-07-20.csv.gz", col_types = "ccicccccci") %>%
  filter(package == "filehash") %>%
  nrow
print(count)
return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
