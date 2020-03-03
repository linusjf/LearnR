#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("skincancer.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  par(mar = c(4, 7, 4, 1))
 plot(data$Lat, data$Mort, main = "Skin cancer mortality versus state latitude",
     xlab = "Latitude (at centre at state)", ylab = "Mortality (Deaths per 10
     million)",
     pch = 19, frame = FALSE)
abline(lm(data$Mort ~ data$Lat, data = data), col = "blue")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
