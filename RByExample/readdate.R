#!/usr/bin/env Rscript
# Goal: To read in a simple data file where date data is present.

# Suppose you have a file 'xdate.data' which looks like this: 1997-07-04,3.1,4
# 1997-07-05,7.2,19 1997-07-07,1.7,2 1997-07-08,1.1,13

main <- function(argv) {
  a <- read.table("xdate.data", sep = ",", col.names = c("date", "my1", "my2"))
  a$date <- as.Date(a$date, format = "%Y-%m-%d", origin = "1970-01-01")

  # Say ?strptime to learn how to use '%' to specify other date formats. Two
  # examples -- '15/12/2002' needs '%d/%m/%Y' '03 Jun 1997' needs '%d %b %Y'

  # Actually, if you're using the ISO 8601 date format, i.e.  '%Y-%m-%d', that's
  # the default setting and you don't need to specify the format.

  # Makes a new column in a
  a$newcol <- a$my1 + a$my2
  # Makes a new R object 'newvar'
  newvar <- a$my1 - a$my2
  print(newvar)
  # Delete the `my1' column
  a$my1 <- NULL
  # Makes summary statistics
  print(summary(a))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
