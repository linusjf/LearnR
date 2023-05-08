#!/usr/bin/env Rscript
library(skimr)
library(Metrics)
suppressMessages(library(dvmisc))

main <- function(argv) {
  data <- read.table("../Data/alcoholarm.txt", header = TRUE)
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(strength ~ alcohol, data = data)
  print(summary(reg))
  onlyintercept <- lm(strength ~ 1, data = data)
  print(summary(onlyintercept))
  anova <- anova(onlyintercept, reg)
  print(anova)

  plot_alcoholarm(data, reg, onlyintercept)
  return(0)
}

plot_alcoholarm <- function(data, reg, onlyintercept) {
  anova <- anova(onlyintercept, reg)
  rss <- round(anova$RSS, 4)
  rss <- c(paste("Model 1 RSS:", rss[1]), paste("Model 2 RSS:", rss[2]))
  p_value <- round(anova$`Pr(>F)`[2])
  p_value <- paste("p-value:", p_value)
  heading <- attr(anova, "heading")[2]
  par(mar = c(4, 7, 4, 1))
  plot(data$alcohol, data$strength, main = paste0(
    "Alcohol versus Strength\n",
    heading
  ), xlab = "Alcohol", ylab = "Strength", pch = 19)
  abline(reg, col = "blue")
  abline(h = mean(data$strength), col = "red", lty = "dashed")
  x0 <- mean(data$alcohol)
  y0 <- mean(data$strength)
  x1 <- x0 + 5
  y1 <- y0 + 2.5
  arrows(x1, y1, x0, y0, angle = 30, code = 2, col = "black", lwd = 4)
  coefs <- reg$coefficients
  text(x1, y1 + 1, paste(round(coefs["(Intercept)"], 4), "\n", round(
    coefs["alcohol"],
    4
  ), "Alcohol"))
  legend("bottomleft", legend = c(rss, p_value), cex = 0.8)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
