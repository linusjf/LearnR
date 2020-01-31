#!/usr/bin/env Rscript
library(assertive.reflection)

main <- function(argv) {
print(Sys.info())
assertive.reflection::assert_is_linux()
assertive.reflection::assert_r_has_libcurl_capability()
assertive.reflection::assert_r_can_compile_code()
return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
