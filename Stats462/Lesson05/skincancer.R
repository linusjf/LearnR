#!/usr/bin/env Rscript
library(skimr)
library(Metrics)
suppressMessages(library(dvmisc))

main <- function(argv) {
  data <- read.table("../Data/skincancer.txt",
    header = TRUE
  )
  print(head(data))
  print(skimr::skim(data))
  reg <- lm(Mort ~ Lat, data = data)
  print(summary(reg))
  onlyintercept <- lm(Mort ~ 1, data = data)
  print(summary(onlyintercept))
  anova <- anova(onlyintercept, reg)
  print(anova)

  plot_skincancer(data, reg, onlyintercept)
  return(0)
}

plot_skincancer <- function(data, reg, onlyintercept) {
  anova <- anova(onlyintercept, reg)
  rss <- round(anova$RSS, 4)
  rss <- c(paste("Model 1 RSS:", rss[1]),
           paste("Model 2 RSS:", rss[2]))
  p_value <- round(anova$`Pr(>F)`[2])
  p_value <- paste("p-value:", p_value)
  heading <- attr(anova, "heading")[2]
  par(mar = c(4, 7, 4, 1))
  plot(data$Lat, data$Mort,
    main = paste0("Latitude versus Mortality\n",
                  heading),
    xlab = "Latitude", ylab = "Mortality",
    pch = 19
  )
  abline(reg, col = "blue")
  abline(h = mean(data$Mort), col = "red", lty = "dashed")
  x0 <- mean(data$Lat)
  y0 <- mean(data$Mort)
  x1 <- x0 + 2.5
  y1 <- y0 + 10
  arrows(x1, y1, x0, y0,
    angle = 30, code = 2, col = "black", lwd = 4
  )
  coefs <- reg$coefficients
  text(x1, y1 + 5, paste(
    round(coefs["(Intercept)"], 4),
    "\n",
    round(coefs["Lat"], 4), "Latitude"
  ))
  legend("bottomleft",
         legend = c(rss, p_value),
         cex = 0.8)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
