#!/usr/bin/env Rscript
# LOGARITHMIC TRANSFORMATION
# compute natural log
islands.ln <- log(islands) 
# Almost looks normal.
boxplot(islands.ln, horizontal = TRUE)

# RANK TRANSFORMATION
# Ranks islands
islands.rank <- rank(islands, ties.method = "random") 
boxplot(islands.rank, horizontal = TRUE)


# DICHOTOMIZATION
# Creates the indicator variable.
continent <- ifelse(islands > 1000, 1, 0) 
# List just the continents.
continent[continent == 1]

# CLEAN UP
# Unloads data sets package.
detach("package:datasets", unload = TRUE) 
# Remove all objects from workspace.
rm(list = ls()) 
