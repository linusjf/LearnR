#!/usr/bin/env Rscript
library(grid)

main <- function(argv) {
  print(head(LifeCycleSavings))
  print(LifeCycleSavings[c("Libya", "Zambia"),])
  print(summary(LifeCycleSavings))
  par(mfrow = c(3, 2), cex = 0.6, mar = c(4, 4, 4, 2), mex = 0.8)
  plot(lm_sr <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings),
    id.n = 1, cex.caption = 0.8, which = 1:6,
    panel = function(...) {
      panel.smooth(..., col.smooth = "gray")
    }
  )
  pairs(LifeCycleSavings,
    panel = panel.smooth,
    main = "LifeCycleSavings data"
  )
  print(summary(lm_sr))
  cat("Printing residuals\n")
  print(resid(lm_sr))
  print(summary(resid(lm_sr)))
  cat("Printing cooks distances\n")
  print(cooks.distance(lm_sr))
  print(summary(cooks.distance(lm_sr)))
  print("Model given level of significance = 0.05")
  par(mfrow = c(3, 2), cex = 0.6, mar = c(4, 4, 4, 2), mex = 0.8)
  plot(lm_sr <- lm(sr ~ pop15 + ddpi, data = LifeCycleSavings),
    id.n = 1, cex.caption = 0.8, which = 1:6,
    panel = function(...) {
      panel.smooth(..., col.smooth = "gray")
    }
  )
  print(summary(lm_sr))
  print("Model given level of significance = 0.01")
  par(mfrow = c(3, 2), cex = 0.6, mar = c(4, 4, 4, 2), mex = 0.8)
  plot(lm_sr <- lm(sr ~ pop15, data = LifeCycleSavings),
    id.n = 1, cex.caption = 0.8, which = 1:6,
    panel = function(...) {
      panel.smooth(..., col.smooth = "gray")
    }
  )
  print(summary(lm_sr))
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
