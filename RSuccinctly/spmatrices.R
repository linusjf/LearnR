#!/usr/bin/env Rscript
# Load the datasets package.
library("datasets")
library("RColorBrewer")
library("car")
# Load data into the workspace.
data(iris)
# Show the first three lines of data.
iris[1:3, ]
# SCATTERPLOT MATRIX WITH DEFAULTS
# Use just the first four variables from iris.
pairs(iris[1:4])

# FUNCTION TO PUT HISTOGRAMS ON DIAGONAL
# Adapted from code in "pairs" help
panel.hist <- function(x, ...) {
  usr <- par("usr") # Copies usr parameters for plot coordinates.
  on.exit(par(usr)) # Restores parameters on exit.
  par(usr = c(usr[1:2], 0, 1.5)) # Sets plot coordinates.
  h <- hist(x, plot = FALSE) # Creates histogram.
  breaks <- h$breaks # Reads breaks for histograms.
  nB <- length(breaks) # Reads number of breaks.
  y <- h$counts # Get raw values for the y-axis.
  y <- y / max(y) # Adjusts raw values to fit the y scale.
  rect(breaks[-nB], 0, breaks[-1], y, ...) # Draws boxes.
}
# SET COLOR PALETTE WITH RCOLORBREWER
# SCATTERPLOT MATRIX WITH OPTIONS
pairs(iris[1:4], # Reads the data.
  panel = panel.smooth,
  # Adds an optional smoother.
  main = "Scatterplot Matrix for Iris Data Using pairs Function",
  diag.panel = panel.hist, # Calls histogram function.
  pch = 16, # Uses solid dots for points.
  # Next line color dots by "Species" category.
  col = brewer.pal(3, "Pastel1")[unclass(iris$Species)]
)

# SCATTERPLOT MATRIX WITH "CAR" PACKAGE
scatterplotMatrix(~ Petal.Length + Petal.Width +
  Sepal.Length + Sepal.Width | Species,
data = iris,
main = paste(
  "Scatterplot Matrix for Iris Data",
  "Using the \"car\" Package"
)
)

# CLEAN UP
# Return to default.
palette("default")
# Unloads the datasets package.
detach("package:datasets", unload = TRUE)
# Unloads RColorBrewer
detach("package:RColorBrewer", unload = TRUE)
# Unloads car package.
detach("package:car", unload = TRUE)
