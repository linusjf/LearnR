#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(rsq))
suppressPackageStartupMessages(library(scatterplot3d))
suppressPackageStartupMessages(library(ggplot2))

main <- function(argv) {
  data <- read.table("../Data/pastry.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))

  print(cor(data))
  print(cor.test(~ Moisture + Sweetness, data))


  reg <- lm(Rating ~ Moisture, data = data)
  print(reg)
  print(anova(reg))

  reg <- lm(Rating ~ Sweetness, data = data)
  print(reg)
  print(anova(reg))

  reg <- lm(Rating ~ Moisture + Sweetness, data = data)
  print(reg)
  print(anova(reg))

  s3d <- scatterplot3d(data,
    main = "Pastry Data",
    axis = TRUE,
    grid = TRUE,
    highlight.3d = TRUE,
    type = "p",
    angle = 45
  )

  p2 <- s3d$xyz.convert(subset(data, data$Sweetness == 2))
  abline(lm(y ~ x, p2))
  p2 <- s3d$xyz.convert(subset(data, data$Sweetness == 4))
  abline(lm(y ~ x, p2))

  labels <- c(
    "Sweetness = 2",
    "Sweetness = 4"
  )

  # save predictions of the model in the new data frame
  # together with variable you want to plot against
  predicted_df <- data.frame(
    Rating = predict(reg, data),
    Moisture = data$Moisture, Sweetness = data$Sweetness
  )

  plot <- data %>%
    ggplot(aes(x = Moisture, y = Rating, color = factor(Sweetness,
      labels =
        labels
    ))) +
    ggtitle("Scatter plot of Rating versus Moisture") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Moisture") +
    ylab("Rating") +
    labs(color = "Sweetness") +
    geom_point(size = 3) +
    geom_line(color = "red", data = subset(predicted_df, Sweetness == 2)) +
    geom_line(color = "green", data = subset(predicted_df, Sweetness == 4))
  print(plot)

  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
