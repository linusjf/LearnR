#!/usr/bin/env Rscript
# Goal: Special cases in reading files

main <- function(argv) {
  b <- data.frame(x1 = runif(100000), x2 = runif(100000), x3 = runif(100000))

  # Write out ascii file:
  write.table(b, file = "./foo.csv", sep = ",", col.names = NA)
  system("gzip -fk foo.csv")
  system("rm foo.csv.bz2; tar -cjf foo.csv.bz2 foo.csv")
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
