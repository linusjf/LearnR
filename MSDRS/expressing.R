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
return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
