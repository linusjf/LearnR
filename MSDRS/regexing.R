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

  print(head(state.name))

  print(grepl(".", "Maryland"))
  print(grepl(".", "*&2[0+,%<@#~|}"))
  print(grepl(".", ""))
  print(grepl("a.b", c("aaa", "aab", "abb", "acadb")))
  print(grepl("a+", "Maryland"))
  print(grepl("x+", "Maryland"))
  print(grepl("x*", "Maryland"))
  print(grepl("s{2}", "Mississippi"))
  print(grepl("ss", "Mississippi"))
  print(grepl("s{2,3}", "Mississippi"))
  print(grepl("i{2,3}", "Mississippi"))
  # nolint start
  print(grepl("(iss){2}", "Mississippi"))
  print(grepl("(ss){2}", "Mississippi"))
  print(grepl("(i.{2}){3}", "Mississippi"))
  # nolint end
  print(grepl("\\w", "abcdefghijklmnopqrstuvwxyz0123456789"))
  print(grepl("\\d", "0123456789"))
  print(grepl("\\s", "\n\t   "))
  print(grepl("\\D", "abcdefghijklmnopqrstuvwxyz"))
  print(grepl("\\w", "\n\t   "))
  print(grepl("[aeiou]", "rhythms"))
  print(grepl("[^aeiou]", "rhythms"))
  print(grepl("[a-m]", "xyz"))
  print(grepl("[a-m]", "ABC"))
  print(grepl("[a-mA-M]", "ABC"))
  print(grepl("\\+", "tragedy + time = humor"))
  print(grepl("\\.", "http://www.jhsph.edu/"))
  print(grepl("^a", c("bab", "aab")))
  print(grepl("b$", c("bab", "aab")))
  print(grepl("^[ab]+$", c("bab", "aab", "abc")))
  print(grepl("^[ab].+$", c("bab", "aab", "abc")))
  print(grepl("a|b", c("abc", "bcd", "cde")))
  print(grepl("North|South", c("South Dakota", "North Carolina", "West Virginia")))

  start_end_vowel <- "^[AEIOU]{1}.+[aeiou]{1}$"
  vowel_state_lgl <- grepl(start_end_vowel, state.name)
  print(head(vowel_state_lgl))
  print(state.name[vowel_state_lgl])
  print(grepl("[Ii]", c("Hawaii", "Illinois", "Kentucky")))
  print(grep("[Ii]", c("Hawaii", "Illinois", "Kentucky")))
  print(sub("[Ii]", "1", c("Hawaii", "Illinois", "Kentucky")))
  print(gsub("[Ii]", "1", c("Hawaii", "Illinois", "Kentucky")))
  two_s <- state.name[grep("ss", state.name)]
  print(two_s)
  print(strsplit(two_s, "ss"))

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
