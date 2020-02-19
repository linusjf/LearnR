#!/usr/bin/env Rscript
#
# Comment:
#
# Code by Arden Miller (Department of Statistics, The University of Auckland).
#
# Lots of coordinate transformations being done "by hand".
# This code is not really reusable;  just a demonstration that very
# pretty results are possible if you're sufficiently keen.
#


main <- function(argv) {
  par(mfrow = c(2, 1), pty = "s", mar = rep(1, 4))
  # Create plotting region and plot outer circle
  plot(c(-1.1, 1.2), c(-1.1, 1.2),
    type = "n", xlab = "", ylab = "",
    xaxt = "n", yaxt = "n", cex.lab = 2.5
  )
  angs <- seq(0, 2 * pi, length = 500)
  xxxx <- sin(angs)
  yyyy <- cos(angs)
  lines(xxxx, yyyy, type = "l")

  # Set constants
  phi1 <- pi * 2 / 9
  k1 <- sin(phi1)
  k2 <- cos(phi1)

  # Create gray regions
  obsphi <- pi / 12
  lambdas <- seq(-pi, pi, length = 500)
  xx <- cos(pi / 2 - obsphi) * sin(lambdas)
  yy <- k2 * sin(pi / 2 - obsphi) - k1 * cos(pi / 2 - obsphi) * cos(lambdas)
  polygon(xx, yy, col = "gray")
  lines(xx, yy, lwd = 2)
  theta1sa <- seq(-obsphi, obsphi, length = 500)
  theta2sa <- acos(cos(obsphi) / cos(theta1sa))
  theta1sb <- seq(obsphi, -obsphi, length = 500)
  theta2sb <- -acos(cos(obsphi) / cos(theta1sb))
  theta1s <- c(theta1sa, theta1sb)
  theta2s <- c(theta2sa, theta2sb)
  xx <- cos(theta1s) * sin(theta2s + pi / 4)
  yy <- k2 * sin(theta1s) - k1 * cos(theta1s) * cos(theta2s + pi / 4)
  polygon(xx, yy, col = "gray")
  lines(xx, yy, lwd = 2)
  xx <- cos(theta1s) * sin(theta2s - pi / 4)
  yy <- k2 * sin(theta1s) - k1 * cos(theta1s) * cos(theta2s - pi / 4)
  polygon(xx, yy, col = "gray")
  lines(xx, yy, lwd = 2)

  # Plot longitudes
  vals <- seq(0, 7, 1) * pi / 8
  for (lambda in vals) {
    sl <- sin(lambda)
    cl <- cos(lambda)
    phi <- atan(((0 - 1) * k2 * cl) / (k1))
    angs <- seq(phi, pi + phi, length = 500)
    xx <- cos(angs) * sl
    yy <- k2 * sin(angs) - k1 * cos(angs) * cl
    lines(xx, yy, lwd = .5)
  }

  # Grey out polar cap
  phi <- 5.6 * pi / 12
  lambdas <- seq(-pi, pi, length = 500)
  xx <- cos(phi) * sin(lambdas)
  yy <- k2 * sin(phi) - k1 * cos(phi) * cos(lambdas)
  polygon(xx, yy, col = "gray")

  # Plot Latitudes
  vals2 <- seq(-2.8, 5.6, 1.4) * pi / 12
  for (phi in vals2) {
    if (k1 * sin(phi) > k2 * cos(phi)) {
      crit <- pi
    } else {
      crit <- acos((-k1 * sin(phi)) / (k2 * cos(phi)))
    }
    lambdas <- seq(-crit, crit, length = 500)
    xx <- cos(phi) * sin(lambdas)
    yy <- k2 * sin(phi) - k1 * cos(phi) * cos(lambdas)
    lines(xx, yy, lwd = .5)
  }


  # Plots axes and label
  lines(c(0.00, 0.00), c(k2 * sin(pi / 2), 1.11), lwd = 4)
  lines(c(0.00, 0.00), c(-1, -1.12), lwd = 4)
  a2x <- sin(-pi / 4)
  a2y <- cos(-pi / 4) * (-k1)
  lines(c(a2x, 1.5 * a2x), c(a2y, 1.5 * a2y), lwd = 4)
  k <- sqrt(a2x^2 + a2y^2)
  lines(c(-a2x / k, 1.2 * (-a2x / k)), c(-a2y / k, 1.2 * (-a2y / k)), lwd = 4)
  a3x <- sin(pi / 4)
  a3y <- cos(pi / 4) * (-k1)
  lines(c(a3x, 1.5 * a3x), c(a3y, 1.5 * a3y), lwd = 4)
  k <- sqrt(a3x^2 + a3y^2)
  lines(c(-a3x / k, 1.2 * (-a3x / k)), c(-a3y / k, 1.2 * (-a3y / k)), lwd = 4)
  text(0.1, 1.12, expression(bold(X[1])))
  text(-1.07, -.85, expression(bold(X[2])))
  text(1.11, -.85, expression(bold(X[3])))

  # set plot region and draw outer circle
  plot(c(-1.1, 1.2), c(-1.1, 1.2),
    type = "n", xlab = "", ylab = "",
    xaxt = "n", yaxt = "n", cex.lab = 2.5
  )
  angs <- seq(0, 2 * pi, length = 500)
  xxxx <- sin(angs)
  yyyy <- cos(angs)
  lines(xxxx, yyyy, type = "l")

  # set constants
  phi1 <- pi * 2 / 9
  k1 <- sin(phi1)
  k2 <- cos(phi1)
  obsphi <- pi / 24

  # create X2X3 gray region and plot boundary
  crit <- acos((-k1 * sin(obsphi)) / (k2 * cos(obsphi)))
  lambdas <- seq(-crit, crit, length = 500)
  xx1 <- cos(obsphi) * sin(lambdas)
  yy1 <- k2 * sin(obsphi) - k1 * cos(obsphi) * cos(lambdas)
  obsphi <- -pi / 24
  crit <- acos((-k1 * sin(obsphi)) / (k2 * cos(obsphi)))
  lambdas <- seq(crit, -crit, length = 500)
  xx3 <- cos(obsphi) * sin(lambdas)
  yy3 <- k2 * sin(obsphi) - k1 * cos(obsphi) * cos(lambdas)
  ang1 <- atan(xx1[500] / yy1[500])
  ang2 <- pi + atan(xx3[1] / yy3[1])
  angs <- seq(ang1, ang2, length = 50)
  xx2 <- sin(angs)
  yy2 <- cos(angs)
  ang4 <- atan(xx1[1] / yy1[1])
  ang3 <- -pi + atan(xx3[500] / yy3[500])
  angs <- seq(ang3, ang4, length = 50)
  xx4 <- sin(angs)
  yy4 <- cos(angs)
  xxa <- c(xx1, xx2, xx3, xx4)
  yya <- c(yy1, yy2, yy3, yy4)
  polygon(xxa, yya, border = "gray", col = "gray")
  xx1a <- xx1
  yy1a <- yy1
  xx3a <- xx3
  yy3a <- yy3

  # create X1X3 gray region and plot boundary
  obsphi <- pi / 24
  crit <- pi / 2 - obsphi
  theta1sa <- c(seq(-crit, crit / 2, length = 200), seq(crit / 2, crit, length =
                                                        500))
  theta2sa <- asin(cos(crit) / cos(theta1sa))
  theta1sb <- seq(crit, crit / 2, length = 500)
  theta2sb <- pi - asin(cos(crit) / cos(theta1sb))
  theta1s <- c(theta1sa, theta1sb)
  theta2s <- c(theta2sa, theta2sb)
  vals <- k1 * sin(theta1s) + k2 * cos(theta1s) * cos(theta2s + pi / 4)
  xx1 <- cos(theta1s[vals >= 0]) * sin(theta2s[vals >= 0] + pi / 4)
  yy1 <- k2 * sin(theta1s[vals >= 0]) - k1 * cos(theta1s[vals >= 0]) *
    cos(theta2s[vals >= 0] + pi / 4)
  theta2s <- -theta2s
  vals <- k1 * sin(theta1s) + k2 * cos(theta1s) * cos(theta2s + pi / 4)
  xx3 <- cos(theta1s[vals >= 0]) * sin(theta2s[vals >= 0] + pi / 4)
  yy3 <- k2 * sin(theta1s[vals >= 0]) - k1 * cos(theta1s[vals >= 0]) *
    cos(theta2s[vals >= 0] + pi / 4)
  rev <- seq(length(xx3), 1, -1)
  xx3 <- xx3[rev]
  yy3 <- yy3[rev]
  ang1 <- pi + atan(xx1[length(xx1)] / yy1[length(yy1)])
  ang2 <- pi + atan(xx3[1] / yy3[1])
  angs <- seq(ang1, ang2, length = 50)
  xx2 <- sin(angs)
  yy2 <- cos(angs)
  ang4 <- pi + atan(xx1[1] / yy1[1])
  ang3 <- pi + atan(xx3[length(xx3)] / yy3[length(yy3)])
  angs <- seq(ang3, ang4, length = 50)
  xx4 <- sin(angs)
  yy4 <- cos(angs)
  xxb <- c(xx1, -xx2, xx3, xx4)
  yyb <- c(yy1, -yy2, yy3, yy4)
  polygon(xxb, yyb, border = "gray", col = "gray")
  xx1b <- xx1
  yy1b <- yy1
  xx3b <- xx3
  yy3b <- yy3

  # create X1X2 gray region and plot boundary
  vals <- k1 * sin(theta1s) + k2 * cos(theta1s) * cos(theta2s - pi / 4)
  xx1 <- cos(theta1s[vals >= 0]) * sin(theta2s[vals >= 0] - pi / 4)
  yy1 <- k2 * sin(theta1s[vals >= 0]) - k1 * cos(theta1s[vals >= 0]) *
    cos(theta2s[vals >= 0] - pi / 4)
  theta2s <- -theta2s
  vals <- k1 * sin(theta1s) + k2 * cos(theta1s) * cos(theta2s - pi / 4)
  xx3 <- cos(theta1s[vals >= 0]) * sin(theta2s[vals >= 0] - pi / 4)
  yy3 <- k2 * sin(theta1s[vals >= 0]) - k1 * cos(theta1s[vals >= 0]) *
    cos(theta2s[vals >= 0] - pi / 4)
  rev <- seq(length(xx3), 1, -1)
  xx3 <- xx3[rev]
  yy3 <- yy3[rev]
  ang1 <- pi + atan(xx1[length(xx1)] / yy1[length(yy1)])
  ang2 <- pi + atan(xx3[1] / yy3[1])
  angs <- seq(ang1, ang2, length = 50)
  xx2 <- sin(angs)
  yy2 <- cos(angs)
  ang4 <- pi + atan(xx1[1] / yy1[1])
  ang3 <- pi + atan(xx3[length(xx3)] / yy3[length(yy3)])
  angs <- seq(ang3, ang4, length = 50)
  xx4 <- sin(angs)
  yy4 <- cos(angs)
  xx <- c(xx1, -xx2, xx3, xx4)
  yy <- c(yy1, -yy2, yy3, yy4)
  polygon(xx, yy, border = "gray", col = "gray")
  xx1c <- xx1
  yy1c <- yy1
  xx3c <- xx3
  yy3c <- yy3


  # plot boundaries to gray regions
  lines(xx1c[2:45], yy1c[2:45], lwd = 2)
  lines(xx1c[69:583], yy1c[69:583], lwd = 2)
  lines(xx1c[660:1080], yy1c[660:1080], lwd = 2)
  lines(xx3c[13:455], yy3c[13:455], lwd = 2)
  lines(xx3c[538:1055], yy3c[538:1055], lwd = 2)
  lines(xx3c[1079:1135], yy3c[1079:1135], lwd = 2)
  lines(xx1a[6:113], yy1a[6:113], lwd = 2)
  lines(xx1a[153:346], yy1a[153:346], lwd = 2)
  lines(xx1a[389:484], yy1a[389:484], lwd = 2)
  lines(xx3a[1:93], yy3a[1:93], lwd = 2)
  lines(xx3a[140:362], yy3a[140:362], lwd = 2)
  lines(xx3a[408:497], yy3a[408:497], lwd = 2)
  lines(xx1b[2:45], yy1b[2:45], lwd = 2)
  lines(xx1b[69:583], yy1b[69:583], lwd = 2)
  lines(xx1b[660:1080], yy1b[660:1080], lwd = 2)
  lines(xx3b[13:455], yy3b[13:455], lwd = 2)
  lines(xx3b[538:1055], yy3b[538:1055], lwd = 2)
  lines(xx3b[1079:1135], yy3b[1079:1135], lwd = 2)

  # Plot longitudes
  vals <- seq(-7, 8, 1) * pi / 8
  for (lambda in vals) {
    sl <- sin(lambda)
    cl <- cos(lambda)
    phi <- atan(((0 - 1) * k2 * cl) / (k1))
    angs <- seq(phi, 5.6 * pi / 12, length = 500)
    xx <- cos(angs) * sl
    yy <- k2 * sin(angs) - k1 * cos(angs) * cl
    lines(xx, yy, lwd = .5)
  }


  # Plot Latitudes
  vals2 <- c(-1.5, 0, 1.5, 3.0, 4.5, 5.6) * pi / 12
  for (phi in vals2) {
    if (k1 * sin(phi) > k2 * cos(phi)) {
      crit <- pi
    } else {
      crit <- acos((-k1 * sin(phi)) / (k2 * cos(phi)))
    }
    lambdas <- seq(-crit, crit, length = 500)
    xx <- cos(phi) * sin(lambdas)
    yy <- k2 * sin(phi) - k1 * cos(phi) * cos(lambdas)
    lines(xx, yy, lwd = .5)
  }


  # create lines for X1X2- and X1X3-planes
  lambda <- pi / 4
  sl <- sin(lambda)
  cl <- cos(lambda)
  phi <- atan(((0 - 1) * k2 * cl) / (k1))
  angs <- seq(phi, pi + phi, length = 500)
  xx <- cos(angs) * sl
  yy <- k2 * sin(angs) - k1 * cos(angs) * cl
  lines(xx, yy, lwd = 2)
  lambda <- 3 * pi / 4
  sl <- sin(lambda)
  cl <- cos(lambda)
  phi <- atan(((0 - 1) * k2 * cl) / (k1))
  angs <- seq(phi, pi + phi, length = 500)
  xx <- cos(angs) * sl
  yy <- k2 * sin(angs) - k1 * cos(angs) * cl
  lines(xx, yy, lwd = 2)

  # create line for X2X3-plane
  phi <- 0
  crit <- acos((-k1 * sin(phi)) / (k2 * cos(phi)))
  lambdas <- seq(-crit, crit, length = 500)
  xx <- cos(phi) * sin(lambdas)
  yy <- k2 * sin(phi) - k1 * cos(phi) * cos(lambdas)
  lines(xx, yy, lwd = 2)

  # create axes
  lines(c(0.00, 0.00), c(k2 * sin(pi / 2), 1.11), lwd = 4)
  lines(c(0.00, 0.00), c(-1, -1.12), lwd = 4)
  a2x <- sin(-pi / 4)
  a2y <- cos(-pi / 4) * (-k1)
  lines(c(a2x, 1.5 * a2x), c(a2y, 1.5 * a2y), lwd = 4)
  a3x <- sin(pi / 4)
  a3y <- cos(pi / 4) * (-k1)
  lines(c(a3x, 1.5 * a3x), c(a3y, 1.5 * a3y), lwd = 4)
  k <- sqrt(a3x^2 + a3y^2)
  lines(c(-a3x / k, 1.2 * (-a3x / k)), c(-a3y / k, 1.2 * (-a3y / k)), lwd = 4)
  k <- sqrt(a2x^2 + a2y^2)
  lines(c(-a2x / k, 1.2 * (-a2x / k)), c(-a2y / k, 1.2 * (-a2y / k)), lwd = 4)


  # add text
  text(-1.07, -.85, expression(bold(X[2])))
  text(1.11, -.85, expression(bold(X[3])))
  text(0.1, 1.12, expression(bold(X[1])))

  lines(xxxx, yyyy, type = "l")
  return(0)
}

if (identical(environment(), globalenv())) {
  quit(status = main(commandArgs(trailingOnly = TRUE)))
}
