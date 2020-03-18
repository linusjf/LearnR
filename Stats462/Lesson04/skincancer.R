#!/usr/bin/env Rscript
library(skimr)

main <- function(argv) {
  data <- read.table("../Data/skincancer.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(Mort ~ Lat, data = data)
  print(reg)
  print(summary(reg))
  print("Mean of Lat")
  print(mean(data$Lat))
  newdata <- data.frame(Lat = c(40, 28, 150))
  print("New Data:")
  print(newdata)
  print("Confidence Interval")
  print(predict(
                reg,
                newdata,
                interval = "confidence"))
  print("Prediction Interval")
  print(predict(
                reg,
                newdata,
                interval = "prediction"))
  return(0)
}


if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
