#!/usr/bin/env Rscript
# Script to be used when upgrading R version

main <- function(argv) {
  print(Sys.info())
  print(sessionInfo())

  if (!is.null(argv) & length(argv) == 2) {
    option <- argv[1]
    platform <- argv[2]
    file <- ''

    switch (platform, "termux"
    = {
      file <- "installed_old.rda"
    }, "archlinux" 
    = {
      file <- "installed_old_arch.rda"
    }
    )
    switch(option, "save"
      = {
        tmp <- installed.packages()
        installedpkgs <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        save(installedpkgs, file = file)
      },
      "upgrade" = {
        old <- options(verbose = TRUE)
        load(file = file)
        tmp <- installed.packages()
        installedpkgs <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        print("Installed packages...")
        print(installedpkgs)
        installedpkgs.new <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        missing <- setdiff(installedpkgs, installedpkgs.new)
        install.packages(missing)
        update.packages(checkBuilt=TRUE)

        load(file)
        tmp <- installed.packages()
        installedpkgs.new <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
        missing <- setdiff(installedpkgs, installedpkgs.new)
        for (i in seq_len(length(missing))) {
          BiocManager::install(missing[i], verbose = TRUE)
        }
        options(old)
      },
      print("usage: upgrade.R save|upgrade termux|archlinux")
    )
  } else {
    print("usage: upgrade.R save|upgrade termux|archlinux")
  }
  return(0)
}

if (identical(environment(), globalenv())) {
  suppressWarnings(quit(status = main(commandArgs(trailingOnly = TRUE))))
}
