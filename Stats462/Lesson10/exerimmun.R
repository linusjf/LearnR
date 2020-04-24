#!/usr/bin/env Rscript
exerimmun.txt <- function() {
  library(rprojroot)
  paste0(
    find_root(has_file(".Rprofile")),
    "/Stats462/Data/exerimmun.txt"
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
source(lib_path())
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(PerformanceAnalytics))

main <- function(argv) {
  cairo_pdf(onefile = TRUE)
  data <- read.table(exerimmun.txt(),
    header = TRUE, as.is = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  chart.Correlation(data,
    histogram = TRUE,
    pch = 15
  )

  model <- lm(igg ~ oxygen, data)
  print(anova(model))
  print(model_coeffs(model))
  print(model_fit_stats(model))
  print(model_equation(model, digits = 4))

  data %<>%
    mutate(oxygensq = oxygen ^ 2)
  chart.Correlation(data,
    histogram = TRUE,
    pch = 15
  )

  model <- lm(igg ~ oxygen + oxygensq, data)
  print(anova(model))
  print(model_coeffs(model))
  stats <- model_fit_stats(model)
  print(stats)
  eqn <- model_equation(model, digits = 4)
  print(eqn)
  with(data,
  plot(oxygen, igg,
  pch = 15, col = "blue",
  main = "Fitted line plot",
  sub = eqn))
  curve(predict(
                model,
                newdata =
                  data.frame(oxygen = oxygen,
                             oxygensq = oxygen ^ 2)),
        add = TRUE, xname = "oxygen")
  labels <- c(
    paste0("Sigma: ", stats$Sigma),
    paste0("R squared: ", stats$R.squared),
    paste0("Adj R squared: ", stats$Adj.R.squared)
  )
  legend("bottomright",
  legend = labels)

  data %<>%
    mutate(oxygencent = c(scale(oxygen))) %>%
    mutate(oxygencentsq = oxygencent ^ 2)
  with(data,
  plot(oxygencent, oxygencentsq,
  pch = 15, col = "blue",
  main = "Scatterplot of oxygencent versus oxygencentsq",
  sub = paste0("Correlation = ",
               round(cor(oxygencent, oxygencentsq), 4)))
  )

  model <- lm(igg ~ oxygencent + oxygencentsq, data)
  print(anova(model))
  print(model_coeffs(model))
  stats <- model_fit_stats(model)
  print(stats)
  eqn <- model_equation(model, digits = 4)
  print(eqn)
  with(data,
  plot(x = oxygencent, y = igg,
  pch = 15, col = "blue",
  main = "Fitted line plot",
  sub = eqn))
  curve(predict(
                model,
                newdata =
                  data.frame(oxygencent = oxygencent,
                             oxygencentsq = oxygencent ^ 2)),
        add = TRUE, xname = "oxygencent")
  labels <- c(
    paste0("Sigma: ", stats$Sigma),
    paste0("R squared: ", stats$R.squared),
    paste0("Adj R squared: ", stats$Adj.R.squared)
  )
  legend("bottomright",
  legend = labels)
  plot(model, which = c(1, 2),
  caption = c("Residuals vs Fitted",
  "Normal Q-Q"),
  main = "LINE conditions met?")

  # predict values
  oxygencent <- scale(data$oxygen)
  scaleList <- list(scale = attr(oxygencent, "scaled:scale"),
    center = attr(oxygencent, "scaled:center"))
  oxygen <- 70
  oxygencent <- (oxygen - scaleList$center) /
    scaleList$scale

  oxygencentsq <- oxygencent ^ 2
  newdata <- data.frame(oxygencent = oxygencent,
  oxygencentsq = oxygencentsq)
  prediction <-
    rbind(
          predict(model, newdata, se.fit = TRUE,
  interval = c("prediction"))$fit,
          predict(model, newdata, se.fit = TRUE,
  interval = c("confidence"))$fit)
  rownames(prediction) <- c("prediction",
                                "confidence")
  print(prediction)

  model <- lm(igg ~ oxygencent, data)
  print(anova(model))
  print(model_coeffs(model))
  stats <- model_fit_stats(model)
  print(stats)
  eqn <- model_equation(model, digits = 4)
  print(eqn)
  with(data,
  plot(x = oxygencent, y = igg,
  pch = 15, col = "blue",
  main = "Fitted line plot",
  sub = eqn))
  curve(predict(
                model,
                newdata =
                  data.frame(oxygencent = oxygencent)),
        add = TRUE, xname = "oxygencent")
  labels <- c(
    paste0("Sigma: ", stats$Sigma),
    paste0("R squared: ", stats$R.squared),
    paste0("Adj R squared: ", stats$Adj.R.squared)
  )
  legend("bottomright",
  legend = labels)

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
