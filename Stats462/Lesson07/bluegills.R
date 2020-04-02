#!/usr/bin/env Rscript
library(scatterplot3d)
suppressPackageStartupMessages(library(e1071))

main <- function(argv) {
  data <- read.table("../Data/bluegills.txt",
    header = TRUE
  )

  print(head(data))
  print(skimr::skim(data))
  # scatter plot
   plot(data,
    type = "p",
    pch = 16,
    main = "Scatterplot Age versus Length",
    sub = "Simple"
  )

  lm <- lm(length ~ age + age ^ 2, data)


  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
