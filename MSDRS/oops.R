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
  structure(list(side_lengths = side_lengths),
            class = "shape_S3")
}

square_4 <- shape_s3(c(4, 4, 4, 4))
print(class(square_4))

triangle_3 <- shape_s3(c(3, 3, 3))
print(class(triangle_3))
print(mean(c(2, 3, 7)))
print(mean(c(as.Date("2016-09-01"),
             as.Date("2016-09-03"))))
print(is_square(square_4))
print(is_square(triangle_3))
print(is_square("square"))
print(is_square(c(1, 1, 1, 1)))
print(square_4)
print(attr(square_4, "class"))
return(0)
}

is_square.default <- function(x) {
  NA
}

is_square <- function(x) UseMethod("is_square")

is_square.shape_S3 <- function(x) {
  length(x$side_lengths) == 4 &&
    x$side_lengths[1] == x$side_lengths[2] &&
    x$side_lengths[2] == x$side_lengths[3] &&
    x$side_lengths[3] == x$side_lengths[4]
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
