#!/usr/bin/env Rscript
# Script to be used when upgrading R version

main <- function(argv) {
  print(Sys.info())
  print(sessionInfo())

  if (!is.null(argv) & length(argv) > 0) {
    option <- argv[1]

    switch(option, "save"
      = {
        tmp <- installed.packages()
        installedpkgs <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        save(installedpkgs, file = "installed_old_arch.rda")
      },
      "upgrade" = {
        old <- options(verbose = TRUE)
        load(file = "installed_old_arch.rda")
        tmp <- installed.packages()
        installedpkgs.new <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        missing <- setdiff(installedpkgs, installedpkgs.new)
        install.packages(missing)
        update.packages()

        load("installed_old_arch.rda")
        tmp <- installed.packages()
        installedpkgs.new <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        missing <- setdiff(installedpkgs, installedpkgs.new)
        for (i in seq_len(length(missing))) {
          BiocManager::install(missing[i], verbose = TRUE)
        }
        options(old)
      },
      print("usage: upgradearch.R save|upgrade")
    )
  } else {
    print("usage: upgradearch.R save|upgrade")
  }
  return(0)
}

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}
