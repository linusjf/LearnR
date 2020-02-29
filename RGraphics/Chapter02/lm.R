#!/usr/bin/env Rscript
library(grid)

main <- function(argv) {
  print(head(LifeCycleSavings))
  print(LifeCycleSavings[c("Libya", "Zambia"), ])
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
  cat(paste(names(resid(lm_sr)), resid(lm_sr)), sep = "\n")
  print(summary(resid(lm_sr)))
  cat("Printing cooks distances\n")
  cat(paste(names(cooks.distance(lm_sr)), cooks.distance(lm_sr)), sep = "\n")
  print(summary(cooks.distance(lm_sr)))
  cat("Printing fitted values\n")
  cat(paste(names(fitted(lm_sr)), fitted(lm_sr)), sep = "\n")
  par(mfrow = c(1, 1), cex = 1, mar = c(4, 4, 4, 2), mex = 1)
  plot(predict(lm_sr), LifeCycleSavings$sr,
    xlab = "predicted", ylab = "actual"
  )
  abline(a = 0, b = 1)
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
