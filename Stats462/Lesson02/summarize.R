#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/houseprice.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  data <- data[["Price"]]
  stem(data)
  hist(data,
    main = "Histogram for House Prices",
    xlab = "Price (Sale price in 000s)",
    xlim = c(150, 400),
    las = 1,
    breaks = c(150, 175, 200, 225, 250, 275, 300, 325, 350, 375, 400)
  )
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
