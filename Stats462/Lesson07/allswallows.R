#!/usr/bin/env Rscript
library(scatterplot3d)

main <- function(argv) {
  data <- read.table("../Data/allswallows.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  # 3D scatter plot
  adults <- subset(data, data$Type == 1)
  lm <- lm(Vent ~ CO2 + O2, adults)
  with(adults, {
         s3d <- scatterplot3d(CO2, O2, Vent,
    type = "p", color = "blue",
    angle = 55, pch = 16,
    main = "Scatterplot 3d Vent_1 vs CO2_1 vs O2_1",
    sub = "Adult swallows"
  )
         s3d$plane3d(lm)
  }
  )
  nestlings <- subset(data, data$Type == 0)
  lm <- lm(Vent ~ CO2 + O2, nestlings)
  with(nestlings, {
         s3d <- scatterplot3d(CO2, O2, Vent,
    type = "p", color = "blue",
    angle = 55, pch = 16,
    main = "Scatterplot 3d Vent vs CO2 vs O2",
    sub = "Nestling swallows"
  )
         s3d$plane3d(lm)
  }
  )
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
