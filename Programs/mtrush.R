#!/usr/bin/env Rscript
library(pixmap)

# adds random noise to img, at the range rows, cols of img; img and the #return
# value are both objects of class pixmap; the parameter q #controls the weight
# of the noise, with the result being 1 - q times the original image
# plus q times the random noise
blurpart <- function(img, rows, cols, q) {
  lrows <- length(rows)
  lcols <- length(cols)
  newimg <- img
  randomnoise <- matrix(nrow = lrows, ncol = lcols, runif(lrows * lcols))
  newimg@grey[rows, cols] <- (1 - q) * img@grey[rows, cols] + q * randomnoise
  return(newimg)
}

main <- function() {
  mtrush1 <- pixmap::read.pnm("mtrush1.pgm")
  mtrush1
  str(mtrush1)
  mtrush1@grey[28, 88]
  pdf("mtrush.pdf")
  plot(mtrush1)
  mtrush2 <- mtrush1
  mtrush2@grey[84:163, 135:177] <- 1
  plot(mtrush2)
  mtrush3 <- blurpart(mtrush1, 84:163, 135:177, 0.65)
  plot(mtrush3)
  graphics.off()
  return(0)
}
