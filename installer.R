#!/usr/bin/env Rscript

install_tools <- function() {
  if (!require(lintr)) {
    install.packages("lintr", .libPaths()[1])
  }
  if (!require(styler)) {
    install.packages("styler", .libPaths()[1])
  }
  if (!require(formatR)) {
    install.packages("formatR", .libPaths()[1])
  }
  if (!require(parallel)) {
    install.packages("parallel", .libPaths()[1])
  }
  if (!require(rex)) {
    install.packages("rex", .libPaths()[1])
  }
  if (!require(devtools)) {
    install.packages("devtools", .libPaths()[1])
  }
  if (!require(roxygen2)) {
    install.packages("roxygen2", .libPaths()[1])
  }
  if (!require(startup)) {
    install.packages("startup", .libPaths()[1])
  }
  if (!require(rprojroot)) {
    install.packages("rprojroot", .libPaths()[1])
  }
  if (!require(githubinstall)) {
    install.packages("githubinstall", .libPaths()[1])
  }
  if (!require(pacman)) {
    install.packages("pacman", .libPaths()[1])
  }
  if (!require(drat)) {
    install.packages("drat", .libPaths()[1])
  }
}

install_rbloggers <- function() {
  if (!require(corrplot)) {
    install.packages("corrplot", .libPaths()[1])
  }
}

install_programs <- function() {
  if (!require(stargazer)) {
    install.packages("stargazer", .libPaths()[1])
  }
  if (!require(compiler)) {
    install.packages("compiler", .libPaths()[1])
  }
  if (!require(rbenchmark)) {
    install.packages("rbenchmark", .libPaths()[1])
  }
  if (!require(pixmap)) {
    install.packages("pixmap", .libPaths()[1])
  }
}

# nolint start
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

install_stats462 <- function() {
  if (!require(skimr)) {
    install.packages("skimr", .libPaths()[1])
  }
  if (!require(shape)) {
    install.packages("shape", .libPaths()[1])
  }
  if (!require(Metrics)) {
    install.packages("Metrics", .libPaths()[1])
  }
  if (!require(dvmisc)) {
    install.packages("dvmisc", .libPaths()[1])
  }
  if (!require(lawstat)) {
    install.packages("lawstat", .libPaths()[1])
  }
  if (!require(RCurl)) {
    install.packages("RCurl", .libPaths()[1])
  }
  if (!require(basicTrendline)) {
    install.packages("basicTrendline", .libPaths()[1])
  }
  if (!require(DescTools)) {
    install.packages("DescTools", .libPaths()[1])
  }
  if (!require(scatterplot3d)) {
    install.packages("scatterplot3d", .libPaths()[1])
  }
  if (!require(plot3D)) {
    install.packages("plot3D", .libPaths()[1])
  }
  if (!require(alr3)) {
    install.packages("alr3", .libPaths()[1])
  }
  if (!require(rsq)) {
    install.packages("rsq", .libPaths()[1])
  }
  if (!require(corrgram)) {
    install.packages("corrgram", .libPaths()[1])
  }
  if (!require(e1071)) {
    install.packages("e1071", .libPaths()[1])
  }
  if (!require(nortest)) {
    install.packages("nortest", .libPaths()[1])
  }
  if (!require(lmtest)) {
    install.packages("lmtest", .libPaths()[1])
  }
  if (!require(VGAM)) {
    install.packages("VGAM", .libPaths()[1])
  }
  if (!require(MASS)) {
    install.packages("MASS", .libPaths()[1])
  }
  remotes::install_github("rsquaredacademy/olsrr", ref = "develop")
  #if (!require(olsrr)) {
   # install.packages("olsrr", .libPaths()[1])
  #}
  if (!require(qualityTools)) {
    install.packages("qualityTools", .libPaths()[1])
  }
  if (!require(tseries)) {
    install.packages("tseries", .libPaths()[1])
  }
  if (!require(forecast)) {
    install.packages("forecast", .libPaths()[1])
  }
  if (!require(Hmisc)) {
    install.packages("Hmisc", .libPaths()[1])
  }
  if (!require(regclass)) {
    install.packages("regclass", .libPaths()[1])
  }
  if (!require(FitAR)) {
    install.packages("FitAR", .libPaths()[1])
  }
  if (!require(orcutt)) {
    install.packages("orcutt", .libPaths()[1])
  }
  if (!require(HoRM)) {
    install.packages("HoRM", .libPaths()[1])
  }
  if (!require(stringi)) {
    install.packages("stringi", .libPaths()[1])
  }
  if (!require(PerformanceAnalytics)) {
    install.packages("PerformanceAnalytics", .libPaths()[1])
  }
  if (!require(tibble)) {
    install.packages("tibble", .libPaths()[1])
  }
  if (!require(leaps)) {
    install.packages("leaps", .libPaths()[1])
  }
  if (!require(reshape)) {
    install.packages("reshape", .libPaths()[1])
  }
  if (!require(stringr)) {
    install.packages("stringr", .libPaths()[1])
  }
}
# nolint end

install_rgraphics <- function() {
  if (!require(RGraphics)) {
    install.packages("RGraphics", .libPaths()[1])
  }
  if (!require(R.devices)) {
    install.packages("R.devices", .libPaths()[1])
  }
  if (!require(colorspace)) {
    install.packages("colorspace", .libPaths()[1])
  }
  if (!require(grImport)) {
    install.packages("grImport", .libPaths()[1])
  }
  if (!require(grid)) {
    install.packages("grid", .libPaths()[1])
  }
  if (!require(lattice)) {
    install.packages("lattice", .libPaths()[1])
  }
  if (!require(ggplot2)) {
    install.packages("ggplot2", .libPaths()[1])
  }
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  if (!require(graph)) {
    BiocManager::install(c("graph"))
  }
  if (!require(pixmap)) {
    install.packages("pixmap")
  }
}

install_rbyexample <- function() {
  if (!require(stringi)) {
    install.packages("stringi", .libPaths()[1])
  }
  if (!require(xtable)) {
    install.packages("xtable", .libPaths()[1])
  }
  if (!require(tseries)) {
    install.packages("tseries", .libPaths()[1])
  }
  if (!require(Hmisc)) {
    install.packages("Hmisc", .libPaths()[1])
  }
  if (!require(foreign)) {
    install.packages("foreign", .libPaths()[1])
  }
  if (!require(StatDataML)) {
    install.packages("StatDataML", .libPaths()[1])
  }
  if (!require(zoo)) {
    install.packages("zoo", .libPaths()[1])
  }
}

install_efficientr <- function() {
  if (!require(microbenchmark)) {
    install.packages("microbenchmark", .libPaths()[1])
  }
  if (!require(profvis)) {
    install.packages("profvis", .libPaths()[1])
  }
  if (!require(ggplot2)) {
    install.packages("ggplot2", .libPaths()[1])
  }
  if (!require(rnoaa)) {
    install.packages("rnoaa", .libPaths()[1])
  }
  if (!require(raster)) {
    install.packages("raster", .libPaths()[1])
  }
  if (!require(benchmarkme)) {
    install.packages("benchmarkme", .libPaths()[1])
  }
  if (!require(assertive.reflection)) {
    install.packages("assertive.reflection", .libPaths()[1])
  }
  if (!require(fortunes)) {
    install.packages("fortunes", .libPaths()[1])
  }
  if (!require(compiler)) {
    install.packages("compiler", .libPaths()[1])
  }
  if (!require(memoise)) {
    install.packages("memoise", .libPaths()[1])
  }
}

# nolint start
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
  print(sessionInfo())
  install_tools()
  install_programs()
  install_rbyexample()
  install_coronavirus()
  install_rgraphics()
  install_rbloggers()
  install_stats462()
  install_efficientr()
  install_msdrs()
  update.packages()
  if (!is.null(warnings())) {
    summary(warnings())
  }
  return(0)
}
# nolint end

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}
