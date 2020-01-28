#!/usr/bin/env Rscript
suppressMessages(library(zoo))
# Goal: Make a time-series object using the "zoo" package
main <- function(argv) {
  a <- data.frame(
    date = c("1995-01-01", "1995-01-02", "1995-01-03", "1995-01-06"),
    x = runif(4),
    y = runif(4)
  )
  a$date <- as.Date(a$date)
  # yyyy-mm-dd is the default format
  # So far there's nothing new - it's just a data frame. I have hand-
  # constructed a but you could equally have obtained it using read.table().

  # I want to make a zoo matrix out of the numerical columns of a
  b <- a
  b$date <- NULL
  z <- zoo::zoo(as.matrix(b), order.by = a$date)
  cat("Print z:\n")
  print(z)
  rm(a, b)

  # So now you are holding "z", a "zoo" object. You can do many cool
  # things with it.
  # See
  # nolint start
  # http://www.google.com/search?hl=en&q=zoo+quickref+achim&btnI=I%27m+Feeling+Lucky
  # nolint end

  # To drop down to a plain data matrix, say
  c <- zoo::coredata(z)
  rownames(c) <- as.character(time(z))
  # Compare --
  cat("Printing c structure:\n")
  print(str(c))
  cat("Printing z structure:\n")
  print(str(z))

  # nolint start
  # The above is a tedious way of doing these things, designed to give you
  # an insight into what is going on. If you just want to read a file
  # into a zoo object, a very short path is something like:
  #        z <- read.zoo(filename, format="%d %b %Y")
  # nolint end
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
