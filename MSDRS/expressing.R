#!/usr/bin/env Rscript

main <- function(argv) {
  two_plus_two <- quote(2 + 2)
  print(two_plus_two)
  print(eval(two_plus_two))
  tpt_string <- "2 + 2"

  tpt_expression <- parse(text = tpt_string)
  print(tpt_expression)

  print(eval(tpt_expression))
  print(deparse(two_plus_two))

  sum_expr <- quote(sum(1, 5))
  print(sum_expr)
  print(eval(sum_expr))

  print(sum_expr[[1]])
  print(sum_expr[[2]])
  print(sum_expr[[3]])
  sum_expr[[1]] <- quote(paste0)
  sum_expr[[2]] <- quote(4)
  sum_expr[[3]] <- quote(6)
  print(eval(sum_expr))

  sum_40_50_expr <- call("sum", 40, 50)
  print(sum_40_50_expr)
  print(sum(40, 50))
  print(eval(sum_40_50_expr))
  print(first_arg(2, 4, "seven", FALSE))
  print(first_arg("two", 4, "seven", FALSE))

  my_new_env <- new.env()
  my_new_env$x <- 4
  print(my_new_env$x)

  assign("y", 9, envir = my_new_env)
  print(get("y", envir = my_new_env))
  print(my_new_env$y)

  print(ls(my_new_env))
  rm("y", envir = my_new_env)
  print(exists("y", envir = my_new_env))
  print(exists("x", envir = my_new_env))
  print(my_new_env$x)
  print(my_new_env$y)

  print(search())
  library(ggplot2)
  print(search())

  print(my_func())
  print(another_func())
  print(assign1())
  print(exists("a_variable_name"))

  assign2()
  print(exists("a_variable_name"))
  return(0)
}

assign2 <- function() {
  a_variable_name <<- "Magic!"
}

my_func <- function() {
  x <- 5
  return(x)
}

assign1 <- function() {
  x <<- "Wow!"
}

another_func <- function() {
  return(x)
}

first_arg <- function(...) {
  expr <- match.call()
  first_arg_expr <- expr[[2]]
  first_arg <- eval(first_arg_expr)
  if (is.numeric(first_arg)) {
    return(paste("The first argument is", first_arg))
  } else {
    return("The first argument is not numeric.")
  }
}

x <- 10
a_variable_name <- NULL
if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
