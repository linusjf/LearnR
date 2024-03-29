#!/usr/bin/env Rscript

install_inferences <- function() {
  if (!require(ggplot2)) {
    install.packages("ggplot2", .libPaths()[1])
  }
  if (!require(tidyr)) {
    install.packages("tidyr", .libPaths()[1])
  }
}

main <- function(argv) {
  print(Sys.info())
  sess <- sessionInfo()
  print(sess)
  platform <- ifelse(is.null(sess$running), "termux", sess$running)
  switch(platform,
    "termux" = {
      Sys.setenv(R_LIBS_USER = paste(Sys.getenv("PREFIX"),"lib/R/library"))
    },
    `Arch Linux ARM` = {
      Sys.setenv(R_LIBS_USER = "/usr/lib/R/library")
    }
  )
  install_inferences()
  if (!is.null(warnings())) {
    summary(warnings())
  }
  return(0)
}

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}
