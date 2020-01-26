#!/usr/bin/env Rscript

main <- function(argv) {
  print(paste("Square", "Circle", "Triangle"))
  print(paste("Square", "Circle", "Triangle", sep = "+"))
  print(paste0("Square", "Circle", "Triangle"))

  shapes <- c("Square", "Circle", "Triangle")
  print(paste("My favorite shape is a", shapes))
  two_cities <- c("best", "worst")
  print(paste("It was the", two_cities, "of times."))
  print(paste(shapes, collapse = " "))
  print(nchar("Supercalifragilisticexpialidocious"))
  cases <- c("CAPS", "low", "Title")
  print(tolower(cases))
  print(toupper(cases))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
