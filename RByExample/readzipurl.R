#!/usr/bin/env Rscript
# Goal: Special cases in reading files

main <- function(argv) {
  # Reading in a .bz2 file --
  cat("Printing head of bz2 file:\n")
  print(head(read.table("./foo.csv.bz2",
    row.names = NULL,
    header = TRUE
  )))
  # Requires you have ./foo.csv.bz2

  # Reading in a .gz file --
  cat("Printing tail of gz file:\n")
  print(tail(read.csv(gzfile("./foo.csv.gz"))))
  # Requires you have ./foo.csv.gz

  # Reading from a pipe --
  cat("Printing contents of awk pipe:\n")
  mydata <- read.table(pipe("awk -f filter.awk foo.csv"))
  print(mydata)

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
