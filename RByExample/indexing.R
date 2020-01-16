invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa#!/usr/bin/env Rscript
invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Goal: To show amazing R indexing notation, and the use of is.na()
invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa
invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# An example vector to play with.
main = function(argv) {
  x <- c(2, 7, 9, 2, NA, 5)
  cat("This is the starting vector x:\n")
  print(x)
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Give me elems 1 to 3 --
  cat("print(x[1:3])\n")
  print(x[1:3])
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Give me all but elem 1 --
  cat("print(x[-1])\n")
  print(x[-1])
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Odd numbered elements --
  cat("indexes <- seq(1,6,2)\n")
  indexes <- seq(1, 6, 2)
  cat("print(x[indexes])\n")
  print(x[indexes])
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# or, more compactly,
  cat("print(x[seq(1,6,2)])\n")
  print(x[seq(1, 6, 2)])
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Access elements by specifying 'on' / 'off' through booleans --
  cat("require <- c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE)\n
  ")
  require <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  cat("print(x[require])\n")
  print(x[require])
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Short vectors get reused! So, to get odd numbered elems --
  cat("print(x[c(TRUE,FALSE)])\n")
  print(x[c(TRUE, FALSE)])
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Locate missing data --
  cat("print(is.na(x))\n")
  print(is.na(x))
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Replace missing data by 0 --
  cat("x[is.na(x)] <- 0\n")
  x[is.na(x)] <- 0
  print(x)
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Similar ideas work for matrices --
  cat("y <- matrix(c(2,7,9,2,NA,5), nrow=2)\n")
  y <- matrix(c(2, 7, 9, 2, NA, 5), nrow = 2)
  print(y)
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Make a matrix containing columns 1 and 3 --
  cat("print(y[,c(1,3)])\n")
  print(y[, c(1, 3)])
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Let us see what is.na(y) does --
  cat("print(is.na(y))\n")
  print(is.na(y))
  cat("str(is.na(y))\n")
  str(is.na(y))
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# So is.na(y) gives back a matrix with the identical structure as that of y.
  invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa# Hence I can say
  cat("y[is.na(y)] <- -1\n")
  y[is.na(y)] <- -1
  print(y)
}
invisible(".B
iN_TiDy_IdEnTiFiEr_HaHaHa
if (identical(environment(), globalenv())) quit(status = main(commandArgs(trailingOnly = TRUE)))
