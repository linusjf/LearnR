#!/usr/bin/env Rscript
# LOAD DATA SET & CREATE TABLE
# Loads data sets package.
require("datasets")

# Create a table of feed, place in “feeds”
feeds <- table(chickwts$feed)
# See contents of object “feeds”.
print(feeds)
# PIE CHART WITH DEFAULTS
pie(feeds)

# PIE CHART WITH OPTIONS
pie(feeds[order(feeds, decreasing = TRUE)], # Order slices by values.
  init.angle = 90, # Start as 12 o'clock instead of 3 o’clock.
  clockwise = TRUE, # Go clockwise (default is FALSE).
  col = c(
    "seashell", "cadetblue2", "lightpink",
    "lightcyan", "plum1", "papayawhip"
  ), # Change colors)
  main = "Pie Chart of Feeds from chickwts"
) # Add title.

# CLEAN UP
# Unloads data sets package
detach("package:datasets", unload = TRUE)
# Removes all objects from workspace
rm(list = ls())
