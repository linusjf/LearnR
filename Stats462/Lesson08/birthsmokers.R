#!/usr/bin/env Rscript
library(skimr)
library(scatterplot3d)
library(plot3D)
suppressPackageStartupMessages(
                               library(dplyr))
library(magrittr)
source("../Lib/libfunc.R")

main <- function(argv) {
  data <- read.table("../Data/birthsmokers.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  scatterplot_matrix(data, "Birth Smokers Scatterplot Matrix")
  data %<>%
    mutate(Smoke = ifelse(data$Smoke == "yes", 1, 0)) %>%
    select(Wgt, Gest, Smoke)
  print(head(data))
  s <- subset(data, data$Smoke == 1)
  lm <- lm(Wgt ~ Gest + Smoke, s)
  # 3D scatter plot
  s3d <- scatterplot3d(data,
    type = "p", color = "blue",
    angle = 55, pch = 16
  )
  s3d$points3d(lm$fitted.values,
  s$Gest, s$Smoke, type = "l")
  ns <- subset(data, data$Smoke == 0)
  lm <- lm(Wgt ~ Gest, ns)
  s3d$points3d(lm$fitted.values,
  ns$Gest, ns$Smoke, type = "l")
  plot3D::scatter3D(data$Wgt,
                    data$Gest,
                    data$Smoke,
  xlab = "Wgt", ylab = "Gest",
  zlab = "Smoke",
  zlim = c(0, 1),
  type = "p", bty = "f", colkey = FALSE,
  ticktype = "detailed")
  s <- subset(data, data$Smoke == 1)
  lm <- lm(Wgt ~ Gest + Smoke, s)
  plot3D::lines3D(lm$fitted.values,
                    s$Gest,
                    s$Smoke,
  zlim = c(0, 1),
  type = "l",
  add = TRUE, colkey = FALSE
  )
  ns <- subset(data, data$Smoke == 0)
  lm <- lm(Wgt ~ Gest, ns)
  plot3D::lines3D(lm$fitted.values,
                    ns$Gest,
                    ns$Smoke,
  zlim = c(0, 1),
  type = "l", add = TRUE, colkey = FALSE
  )
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
