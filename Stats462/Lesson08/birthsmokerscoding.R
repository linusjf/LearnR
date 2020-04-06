#!/usr/bin/env Rscript
birthsmokers.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/birthsmokers.txt"
  )
}

lib_path <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Lib/libfunc.R"
  )
}

library(skimr)
library(scatterplot3d)
library(plot3D)
suppressPackageStartupMessages(
  library(dplyr)
)
library(magrittr)
source(lib_path())


main <- function(argv) {
  data <- read.table(birthsmokers.txt(),
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  scatterplot_matrix(data, "Birth Smokers Scatterplot Matrix")
  data %<>%
    mutate(Smoke = ifelse(data$Smoke == "yes", 1, -1))
  print(head(data))
  s <- subset(data, data$Smoke == 1)
  lm <- lm(Wgt ~ Gest + Smoke, s)
  # 3D scatter plot
  s3d <- scatterplot3d(data,
    type = "p", color = "blue",
    angle = 55, pch = 16
  )
  s3d$points3d(lm$fitted.values,
    s$Gest, s$Smoke,
    type = "l"
  )
  ns <- subset(data, data$Smoke == -1)
  lm <- lm(Wgt ~ Gest, ns)
  s3d$points3d(lm$fitted.values,
    ns$Gest, ns$Smoke,
    type = "l"
  )
  plot3D::scatter3D(data$Wgt,
    data$Gest,
    data$Smoke,
    xlab = "Wgt", ylab = "Gest",
    zlab = "Smoke",
    zlim = c(-1, 1),
    type = "p", bty = "f", colkey = FALSE,
    ticktype = "detailed"
  )
  s <- subset(data, data$Smoke == 1)
  lm <- lm(Wgt ~ Gest + Smoke, s)
  plot3D::lines3D(lm$fitted.values,
    s$Gest,
    s$Smoke,
    type = "l",
    add = TRUE, colkey = FALSE
  )
  ns <- subset(data, data$Smoke == -1)
  lm <- lm(Wgt ~ Gest, ns)
  plot3D::lines3D(lm$fitted.values,
    ns$Gest,
    ns$Smoke,
    type = "l", add = TRUE, colkey = FALSE
  )

  lm <- lm(Wgt ~ Gest + Smoke, data)
  complete <- complete_anova(lm)
  print(complete)
  anova <- anova(lm)
  print(anova)
  summ <- summary(lm)
  print(summ)
  smokecoeff <- lm$coefficients["Smoke"]
  cis <- confint(lm, "Smoke")
  fit <- c(smokecoeff, cis)
  names(fit) <- c("Value", "Lower", "Upper")
  print(fit)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
