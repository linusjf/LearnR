#!/usr/bin/env Rscript
require(stats)
require(graphics)

boxplot(weight ~ feed,
  data = chickwts, col = "lightgray",
  varwidth = TRUE, notch = FALSE, main = "chickwt data",
  ylab = "Weight at six weeks (gm)"
)
anova(fm1 <- lm(weight ~ feed, data = chickwts))
opar <- par(
  mfrow = c(2, 2),
  oma = c(0, 0, 1.1, 0),
  mar = c(4.1, 4.1, 2.1, .1)
)
plot(fm1)
par(opar)
