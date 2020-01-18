#!/usr/bin/env Rscript
suppressMessages(library(Hmisc))
# Goal: To read in a simple data file, and look around it's contents.

# Suppose you have a file "x.data" which looks like this:
#        1997,3.1,4
#        1998,7.2,19
#        1999,1.7,2
#        2000,1.1,13
# To read it in --

main <- function(argv) {
  a <- read.table("x.data",
    sep = ",",
    col.names = c("year", "my1", "my2")
  )
  cat("Number of rows:\n\n")
  print(nrow(a))
  # Count the rows in a

  cat("Summary:\n\n")
  print(summary(a$year))
  # The column "year" in data frame a
  # is accessed as a$year

  # Makes a new column in a
  a$newcol <- a$my1 + a$my2
  # Makes a new R object "newvar"
  newvar <- a$my1 - a$my2
  cat(newvar, "\n")
  # Removes the column "my1"
  a$my1 <- NULL

  # You might find these useful, to "look around" a dataset --
  cat("\nStructure:\n\n")
  str(a)
  cat("\nSummary:\n\n")
  print(summary(a))
  # This requires that you've installed the Hmisc package
  cat("\nContents:\n\n")
  print(Hmisc::contents(a))
  cat("Describe:\n\n")
  print(Hmisc::describe(a))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
