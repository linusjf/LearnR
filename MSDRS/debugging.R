#!/usr/bin/env Rscript
check_n_value <- function(n) {
  if (n > 0) {
    stop("n should be <= 0")
  }
}

# nolint start
error_if_n_is_greater_than_zero <- function(n) {
  check_n_value(n)
  return(n)
}
# nolint end
trace(check_n_value)
error_if_n_is_greater_than_zero(5)
