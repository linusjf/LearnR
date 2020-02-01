#!/usr/bin/env Rscript

beera <- function(expr) {
  tryCatch(expr,
         error = function(e) {
           message("An error occurred:\n", e)
         },
         warning = function(w) {
           message("A warning occured:\n", w)
         },
         finally = {
           message("Finally done!")
         })
}

main <- function(args) {
print(beera({
  2 + 2
}))
print(beera({
  "two" + 2
}))
print(beera({
  as.numeric(c(1, "two", 3))
}))
return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
