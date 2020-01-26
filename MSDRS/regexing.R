#!/usr/bin/env Rscript

main <- function(argv) {
regular_expression <- "a"
string_to_search <- "Maryland"
print(grepl(regular_expression, string_to_search))
regular_expression <- "u"
string_to_search <- "Maryland"
print(grepl(regular_expression, string_to_search))

print(grepl("land", "Maryland"))
print(grepl("ryla", "Maryland"))
print(grepl("Marly", "Maryland"))
print(grepl("dany", "Maryland"))
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
