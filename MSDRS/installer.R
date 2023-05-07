#!/usr/bin/env Rscript

install_msdrs <- function() {
  if (!require(bookdown)) {
    install.packages("bookdown", .libPaths()[1])
  }
  if (!require(data.table)) {
    install.packages("data.table", .libPaths()[1])
  }
  if (!require(datasets)) {
    install.packages("datasets", .libPaths()[1])
  }
  if (!require(devtools)) {
    install.packages("devtools", .libPaths()[1])
  }
  if (!require(dlnm)) {
    install.packages("dlnm", .libPaths()[1])
  }
  if (!require(dplyr)) {
    install.packages("dplyr", .libPaths()[1])
  }
  if (!require(faraway)) {
    install.packages("faraway", .libPaths()[1])
  }
  if (!require(forcats)) {
    install.packages("forcats", .libPaths()[1])
  }
  if (!require(GGally)) {
    install.packages("GGally", .libPaths()[1])
  }
  if (!require(ggmap)) {
    install.packages("ggmap", .libPaths()[1])
  }
  if (!require(ggplot2)) {
    install.packages("ggplot2", .libPaths()[1])
  }
  if (!require(ggthemes)) {
    install.packages("ggthemes", .libPaths()[1])
  }
  if (!require(ghit)) {
    install.packages("ghit", .libPaths()[1])
  }
  if (!require(grid)) {
    install.packages("grid", .libPaths()[1])
  }
  if (!require(gridExtra)) {
    install.packages("gridExtra", .libPaths()[1])
  }
  if (!require(httr)) {
    install.packages("httr", .libPaths()[1])
  }
  if (!require(knitr)) {
    install.packages("knitr", .libPaths()[1])
  }
  if (!require(leaflet)) {
    install.packages("leaflet", .libPaths()[1])
  }
  if (!require(lubridate)) {
    install.packages("lubridate", .libPaths()[1])
  }
  if (!require(magrittr)) {
    install.packages("magrittr", .libPaths()[1])
  }
  if (!require(methods)) {
    install.packages("methods", .libPaths()[1])
  }
  if (!require(microbenchmark)) {
    install.packages("microbenchmark", .libPaths()[1])
  }
  if (!require(package)) {
    install.packages("package", .libPaths()[1])
  }
  if (!require(pander)) {
    install.packages("pander", .libPaths()[1])
  }
  if (!require(plotly)) {
    install.packages("plotly", .libPaths()[1])
  }
  if (!require(profvis)) {
    install.packages("profvis", .libPaths()[1])
  }
  if (!require(pryr)) {
    install.packages("pryr", .libPaths()[1])
  }
  if (!require(purrr)) {
    install.packages("purrr", .libPaths()[1])
  }
  if (!require(R.devices)) {
    install.packages("R.devices", .libPaths()[1])
  }
  if (!require(rappdirs)) {
    install.packages("rappdirs", .libPaths()[1])
  }
  if (!require(raster)) {
    install.packages("raster", .libPaths()[1])
  }
  if (!require(RColorBrewer)) {
    install.packages("RColorBrewer", .libPaths()[1])
  }
  if (!require(readr)) {
    install.packages("readr", .libPaths()[1])
  }
  if (!require(rmarkdown)) {
    install.packages("rmarkdown", .libPaths()[1])
  }
  if (!require(scales)) {
    install.packages("scales", .libPaths()[1])
  }
  if (!require(sp)) {
    install.packages("sp", .libPaths()[1])
  }
  if (!require(stats)) {
    install.packages("stats", .libPaths()[1])
  }
  if (!require(stringr)) {
    install.packages("stringr", .libPaths()[1])
  }
  if (!require(testthat)) {
    install.packages("testthat", .libPaths()[1])
  }
  if (!require(tibble)) {
    install.packages("tibble", .libPaths()[1])
  }
  if (!require(tidyr)) {
    install.packages("tidyr", .libPaths()[1])
  }
  if (!require(tidyverse)) {
    install.packages("tidyverse", .libPaths()[1])
  }
  if (!require(titanic)) {
    install.packages("titanic", .libPaths()[1])
  }
  if (!require(viridis)) {
    install.packages("viridis", .libPaths()[1])
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
  install_msdrs()
  if (!is.null(warnings())) {
    summary(warnings())
  }
  return(0)
}
# nolint end

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}
