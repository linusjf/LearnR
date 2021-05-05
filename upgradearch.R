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
        print(tmp)
        installedpkgs <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        print("Saving installed packages...")
        print(installedpkgs)
        save(installedpkgs, file = "installed_old_arch.rda")
      },
      "upgrade" = {
        old <- options(verbose = TRUE)
        load(file = "installed_old_arch.rda")
        tmp <- installed.packages()
        installedpkgs <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        print("Installed packages...")
        print(installedpkgs)
        installedpkgs.new <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        missing <- setdiff(installedpkgs, installedpkgs.new)
        print("Missing packages...")
        print(missing)
        install.packages(missing)
        update.packages(checkBuilt=TRUE)

        load("installed_old_arch.rda")
        tmp <- installed.packages()
        installedpkgs <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        installedpkgs.new <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        missing <- setdiff(installedpkgs, installedpkgs.new)
        print("Missing packages...")
        print(missing)
        for (i in seq_len(length(missing))) {
          BiocManager::install(missing[i])
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
