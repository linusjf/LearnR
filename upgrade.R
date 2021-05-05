#!/usr/bin/env Rscript
# Script to be used when upgrading R version

main <- function(argv) {
  print(Sys.info())
  sess <- sessionInfo()
  print(sess)
  platform <- ifelse(is.null(sess$running), "termux", sess$running)

  if (!is.null(argv) & length(argv) > 0) {
    option <- argv[1]
    file <- NULL

    switch(platform,
      "termux" = {
        file <- "installed_old.rda"
        Sys.setenv(R_LIBS_USER = "/data/data/com.termux/files/usr/lib/R/library")
      },
      `Arch Linux ARM` = {
        file <- "installed_old_arch.rda"
        Sys.setenv(R_LIBS_USER = "/usr/lib/R/library")
      }
    )
    switch(option,
      save = {
        tmp <- installed.packages()
        installedpkgs <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        save(installedpkgs, file = file)
      },
      upgrade = {
        old <- options(verbose = TRUE)
        load(file = file)
        tmp <- installed.packages()
        installedpkgs <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        print("Installed packages...")
        print(installedpkgs)
        installedpkgs.new <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        missing <- setdiff(installedpkgs, installedpkgs.new)
        install.packages(missing)
        update.packages(
          checkBuilt = TRUE,
          oldPkgs = old.packages()
        )

        load(file)
        tmp <- installed.packages()
        installedpkgs.new <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        missing <- setdiff(installedpkgs, installedpkgs.new)
        for (i in seq_len(length(missing))) {
          BiocManager::install(missing[i], verbose = TRUE)
        }
        options(old)
      },
      print("usage: upgrade.R save|upgrade")
    )
  } else {
    print("usage: upgrade.R save|upgrade")
  }
  return(0)
}

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}
