#!/usr/bin/env Rscript
library(stringr)

main <- function(argv) {
state_tbl <- paste(state.name, state.area, state.abb)
print(head(state_tbl))
print(stringr::str_extract(state_tbl, "[0-9]+"))
print(head(state.name))
print(stringr::str_order(state.name))
print(head(state.abb))
print(stringr::str_order(state.abb))
print(stringr::str_pad("Thai", width = 8, side = "left", pad = "-"))
print(stringr::str_pad("Thai", width = 8, side = "both", pad = "-"))
cases <- c("CAPS", "low", "Title")
print(stringr::str_to_title(cases))
to_trim <- c("   space", "the    ", "    final frontier  ")
print(stringr::str_trim(to_trim))
pasted_states <- paste(state.name[1:20], collapse = " ")

cat(stringr::str_wrap(pasted_states, width = 80))
cat(stringr::str_wrap(pasted_states, width = 30))
a_tale <- "It was the best of times it was the worst of times it was the age of
wisdom it was the age of foolishness"
print(stringr::word(a_tale, 2))
print(stringr::word(a_tale, end = 3))
print(stringr::word(a_tale, start = 11, end = 15))

return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
