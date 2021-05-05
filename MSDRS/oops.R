#!/usr/bin/env Rscript

main <- function(argv) {
  print(class(2))
  print(class("is in session."))
  print(class(class))
  special_num_1 <- structure(1, class = "special_number")
  print(class(special_num_1))

  special_num_2 <- 2
  print(class(special_num_2))
  class(special_num_2) <- "special_number"
  print(class(special_num_2))

  shape_s3 <- function(side_lengths) {
    structure(list(side_lengths = side_lengths), class = "shape_S3")
  }

  square_4 <- shape_s3(c(4, 4, 4, 4))
  print(class(square_4))

  triangle_3 <- shape_s3(c(3, 3, 3))
  print(class(triangle_3))
  print(mean(c(2, 3, 7)))
  print(mean(c(as.Date("2016-09-01"), as.Date("2016-09-03"))))
  print(is_square(square_4))
  print(is_square(triangle_3))
  print(is_square("square"))
  print(is_square(c(1, 1, 1, 1)))
  print(square_4)
  print(attr(square_4, "class"))
  print(square_4)
  print(triangle_3)
  print(shape_s3(c(10, 10, 20, 20, 15)))
  print(shape_s3(c(2, 3, 4, 5)))
  print(head(methods(print, 10)))
  print(class(square_4))
  class(square_4) <- c("shape_S3", "square")
  print(class(square_4))
  print(inherits(square_4, "square"))
  return(0)
}

print.shape_S3 <- function(x) {
  if (length(x$side_lengths) == 3) {
    print(paste("A triangle with side lengths of", x$side_lengths[1], ",", x$side_lengths[2], 
      "and", x$side_lengths[3]))
  } else if (length(x$side_lengths) == 4) {
    if (is_square(x)) {
      print(paste("A square with length size", x$side_lengths[1]))
    } else {
      print(paste("A quadrilateral with side lengths of", x$side_lengths[1], 
        ",", x$side_lengths[2], ",", x$side_lengths[3], "and", x$side_lengths[4]))
    }
  } else {
    print(paste("A shape with", length(x$side_lengths), "sides."))
  }
}

is_square.default <- function(x) {
  NA
}

is_square <- function(x) UseMethod("is_square")

is_square.shape_S3 <- function(x) {
  length(x$side_lengths) == 4 && x$side_lengths[1] == x$side_lengths[2] && x$side_lengths[2] == 
    x$side_lengths[3] && x$side_lengths[3] == x$side_lengths[4]
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
