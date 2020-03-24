#!/usr/bin/env Rscript

par(xpd = NA, mar = c(3, 3, 2, 8))
plot(-4:4, (-4:4)^2, pch = 1:2, ann = FALSE)
legend(c("odd", "even"), pch = 1:2, x = 5, y = 4^2)
