#!/usr/bin/env Rscript
# Goal: All manner of import and export of datasets.
library(foreign)
library(StatDataML)

main <- function(argv) {
  # Invent a dataset --
  a <- data.frame(
    name = c("a", "b", "c"),
    ownership = c("Case 1", "Case 1", "Case 2"),
    listed.at = c("NSE", NA, "BSE"),
    # Firm "b" is unlisted.
    is.listed = c(TRUE, FALSE, TRUE),
    # R convention - boolean variables are named "is.something"
    x = c(2.2, 3.3, 4.4),
    date = as.Date(c("2004-04-04", "2005-05-05", "2006-06-06"))
  )

  # To a spreadsheet through a CSV file --
  write.table(a,
              file = "demo.csv",
              sep = ",",
              col.names = NA,
              qmethod = "double")
  b <- read.table("demo.csv",
                  header = TRUE,
                  sep = ",",
                  row.names = 1)

  # To R as a binary file --
  save(a, file = "demo.rda")
  load("demo.rda")

  # To the Open XML standard for transport for statistical data --
  StatDataML::writeSDML(a, "demo.sdml")
  b <- StatDataML::readSDML("demo.sdml")

  # To Stata --
  foreign::write.dta(a, "demo.dta")
  b <- foreign::read.dta("demo.dta")

  # foreign::write.foreign() also has a pathway to SAS and SPSS.
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
