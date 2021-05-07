#!/usr/bin/env Rscript
# LOAD DATA
# Loads data sets package
library("datasets")

# DEFAULT CHART WITH PLOT()
# Default method to plot the variable feed from chickwts
plot(chickwts$feed)

# CREATE TABLE
feeds <- table(chickwts$feed) # Create a table of feed, place in “feeds”
# See contents of object “feeds”
feeds
# Identical to plot(chickwts$feed) but with new object
barplot(feeds)

# USE BARPLOT() AND PAR() FOR PARAMETERS
# Stores current graphical parameters.
oldpar <- par(no.readonly = TRUE)
par(oma = c(1, 4, 1, 1)) # Sets outside margins: bottom, left, top, right.
par(mar = c(4, 5, 2, 1)) # Sets plot margins.
barplot(feeds[order(feeds)], # Orders the bars by descending values.
  horiz = TRUE, # Makes the bars horizontal.
  las = 1, # las gives orientation of axis labels.
  col = c(
    "beige", "blanchedalmond", "bisque1", "bisque2",
    "bisque3", "bisque4"
  ), # Vector of colors for bars.
  border = NA, # No borders on bars.
  # Add main title and label for x-axis.
  main = "Frequencies of Different Feeds in chickwts Data set",
  xlab = "Number of Chicks"
)

# CLEAN UP
# Restores previous parameters (ignore errors).
par(oldpar)
# Unloads data sets package.
detach("package:datasets", unload = TRUE)
# Removes all objects from workspace.
rm(list = ls())
