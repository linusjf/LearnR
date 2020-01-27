#!/usr/bin/env Rscript

main <- function() {
# nolint start
dont_lint_me <- "xyz == NA shouldn't occur in an R script"
print(dont_lint_me)
x <- NA
# use is.na instead of x == NA
# nolint end
dont_lint_me_either <- is.na(x)
print(dont_lint_me_either)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
