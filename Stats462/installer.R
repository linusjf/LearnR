#!/usr/bin/env Rscript

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
  # if (!require(olsrr)) { install.packages('olsrr', .libPaths()[1]) }
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
  if (!require(survey)) {
    install.packages("survey", .libPaths()[1])
  }
  if (!require(glmulti)) {
    install.packages("glmulti", .libPaths()[1])
  }
  if (!require(bestglm)) {
    install.packages("bestglm", .libPaths()[1])
  }
  if (!require(finalfit)) {
    install.packages("finalfit", .libPaths()[1])
  }
  if (!require(oddsratio)) {
    install.packages("oddsratio", .libPaths()[1])
  }
  if (!require(ResourceSelection)) {
    install.packages("ResourceSelection", .libPaths()[1])
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
  install_stats462()
  if (!is.null(warnings())) {
    summary(warnings())
  }
  return(0)
}
# nolint end

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}
