#!/usr/bin/env Rscript

install_coronavirus <- function() {
  if (!require(nCov2019)) {
    remotes::install_github("GuangchuangYu/nCov2019")
  }
  if (!require(maps)) {
    install.packages("maps", .libPaths()[1])
  }
  if (!require(mapdata)) {
    install.packages("mapdata", .libPaths()[1])
  }
  if (!require(mapproj)) {
    install.packages("mapproj", .libPaths()[1])
  }
  if (!require(partykit)) {
    install.packages("partykit", .libPaths()[1])
  }
  if (!require(ipred)) {
    install.packages("ipred", .libPaths()[1])
  }
  if (!require(magrittr)) {
    install.packages("magrittr", .libPaths()[1])
  }
  if (!require(lubridate)) {
    install.packages("lubridate", .libPaths()[1])
  }
  if (!require(tidyverse)) {
    install.packages("tidyverse", .libPaths()[1])
  }
  if (!require(gridExtra)) {
    install.packages("gridExtra", .libPaths()[1])
  }
  if (!require(kableExtra)) {
    install.packages("kableExtra", .libPaths()[1])
  }
  if (!require(magick)) {
    install.packages("magick", .libPaths()[1])
  }
  if (!require(deSolve)) {
    install.packages("deSolve", .libPaths()[1])
  }
  if (!require(EpiDynamics)) {
    install.packages("EpiDynamics", .libPaths()[1])
  }
  if (!require(EpiModel)) {
    install.packages("EpiModel", .libPaths()[1])
  }
  if (!require(scales)) {
    install.packages("scales", .libPaths()[1])
  }
  if (!require(tidyr)) {
    install.packages("tidyr", .libPaths()[1])
  }
  if (!require(formattable)) {
    install.packages("formattable", .libPaths()[1])
  }
}

main <- function(argv) {
  print(Sys.info())
  sess <- sessionInfo()
  print(sess)
  platform <- ifelse(is.null(sess$running), "termux", sess$running)
  switch(platform,
    "termux" = {
      Sys.setenv(R_LIBS_USER = "/data/data/com.termux/files/usr/lib/R/library")
    },
    `Arch Linux ARM` = {
      Sys.setenv(R_LIBS_USER = "/usr/lib/R/library")
    }
  )
  install_coronavirus()
  if (!is.null(warnings())) {
    summary(warnings())
  }
  return(0)
}

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}
