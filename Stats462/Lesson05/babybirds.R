#!/usr/bin/env Rscript
library(scatterplot3d)
source("libfunc.R")

main <- function(argv) {
  data <- read.table("../Data/babybirds.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  scatterplot_matrix(data, "Baby Birds Scatterplot Matrix")
  # 3D scatter plot
s3d <- scatterplot3d(data, type = "h", color = "blue",
    angle = 55, pch = 16)
# Add regression plane
lm <- lm(Vent ~ O2 + CO2, data)
s3d$plane3d(lm)
s3d <- scatterplot3d(data, type = "h", color = "blue",
    angle = 135, pch = 16)
s3d$plane3d(lm)
s3d <- scatterplot3d(data, type = "h", color = "blue",
    angle = 90, pch = 16)
s3d$plane3d(lm)
print(complete_anova(lm))
return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
