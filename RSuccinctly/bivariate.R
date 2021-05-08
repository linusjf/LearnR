#!/usr/bin/env Rscript
library("datasets") 
# Load the datasets package.
# Load data into the workspace.
data(trees) 
# Show the first 5 lines.
trees[1:5, ]
# GRAPHICAL CHECK
hist(trees$Height)
hist(trees$Girth)
plot(trees$Girth, trees$Height)

# BASIC REGRESSION MODEL
# Save the model.
reg1 <- lm(Height ~ Girth, data = trees) 
# Get regression output.
summary(reg1)

# CONFIDENCE INTERVALS
confint(reg1)

# CLEAN UP
# Unloads data sets package.
detach("package:datasets", unload = TRUE) 
# Remove all objects from workspace.
rm(list = ls()) 
