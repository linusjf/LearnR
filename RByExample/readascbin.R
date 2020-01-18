#!/usr/bin/env Rscript
# Goal: Reading and writing ascii files, reading and writing binary files.
#       And, to measure how much faster it is working with binary files.

# First manufacture a tall data frame:
# FYI -- runif(10) yields 10 U(0,1) random numbers.
main <- function(argv) {
  b <- data.frame(x1 = runif(100000), x2 = runif(100000), x3 = runif(100000))
  print(summary(b))

  # Write out ascii file:
  write.table(b, file = "./foo.csv", sep = ",", col.names = NA)
  # Read in this resulting ascii file:
  c <- read.table("./foo.csv", header = TRUE, sep = ",", row.names = 1)
  # Write a binary file out of dataset C:
  save(c, file = "./foo.binary")
  # Delete the dataset C:
  rm(c)
  # Restore from foo.binary:
  load("./foo.binary")
  print(summary(c))
  # should yield the same results
  # as summary(b) above.

  # Now we time all these operations --
  cat("Time creation of dataset:\n")
  print(system.time({
    b <- data.frame(x1 = runif(100000), x2 = runif(100000), x3 = runif(100000))
  }))

  cat("Time writing an ascii file out of dataset b:\n")
  print(system.time(
    write.table(b, file = "./foo.csv", sep = ",", col.names = NA)
  ))

  cat("Time reading an ascii file into dataset c:\n")
  print(system.time({
    c <- read.table("./foo.csv",
      header = TRUE,
      sep = ",",
      row.names = 1
    )
  }))

  cat("Time writing a binary file out of dataset c:\n")
  print(system.time(save(c, file = "./foo.binary")))

  cat("Time reading a binary file + variablenames from ./foo.binary:\n")
  print(system.time(load("./foo.binary")))
  # and then read it in from binary file
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
