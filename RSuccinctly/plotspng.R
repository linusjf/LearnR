#!/usr/bin/env Rscript 
# LOAD DATA
# Loads data sets package
require("datasets") 
# CREATE TABLE
# Create a table of feed, place in “feeds”
feeds <- table(chickwts$feed) 
# See contents of object “feeds”
print(feeds) 
# CHOOSE GRAPHICS DEVICE
# TO SAVE AS PNG
# EITHER this device for a PNG file (raster graphics)
png(filename = "plots.png", # Give the full path and name.
 width = 900, # Width of image in pixels.
 height = 600) # Height of image in pixels.

# CREATE GRAPHIC
# Then run the command(s) for the graphic.
oldpar <- par(no.readonly = TRUE) # Stores current graphical parameters.
par(oma = c(1, 1, 1, 1)) # Sets outside margins: bottom, left, top, right.
par(mar = c(4, 5, 2, 1)) # Sets plot margins.
barplot(feeds[order(feeds)], # Order the bars by descending values.
 horiz = TRUE, # Make the bars horizontal.
 las = 1, # las gives orientation of axis labels.
 col = c("beige", "blanchedalmond", "bisque1", "bisque2",
         "bisque3", "bisque4"), # Vector of colors for bars
 border = NA, # No borders on bars.
 # Add main title and label for x-axis.
 main = "Frequencies of Different Feeds\nin chickwts Data set",
 xlab = "Number of Chicks") 
# CLEAN UP
# Turns off graphics device.
invisible(dev.off()) 
# Restores previous graphics parameters (ignore errors).
par(oldpar) 
# Removes all objects from workspace.
rm(list = ls()) 
