#!/usr/bin/env Rscript
# LOAD DATA
# Load the datasets package.
library("datasets") 
# Show complete data in tables.
print("Titanic")
print(Titanic) 
# Display a "flat" contingency table.
ftable(Titanic)
# RESTRUCTURE DATA
tdf <- as.data.frame(lapply(as.data.frame.table(Titanic),
 function(x)rep(x, as.data.frame.table(Titanic)$Freq)))[, -5]
# Check the first five rows of data.
print("First five rows:")
print(tdf[1:5, ]) 
# CREATE REDUCED TABLE
# Select two variables.
ttab <- table(tdf$Class, tdf$Survived) 
# Show the new table.
print("New table:")
print(ttab)
# PERCENTAGES
# row %
rowp <- round(prop.table(ttab, 1), 2) * 100 
# column %
colp <- round(prop.table(ttab, 2), 2) * 100 
# total %
totp <- round(prop.table(ttab), 2) * 100 
print("row %:")
print(rowp)
print("col %:")
print(colp)
print("tot %:")
print(totp)
# Compare two variables in ttab
tchi <- chisq.test(ttab) 
print(tchi)

# CLEAN UP
# Unloads the datasets package.
detach("package:datasets", unload = TRUE) 
# Remove all objects from the workspace
rm(list = ls()) 
