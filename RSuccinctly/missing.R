#!/usr/bin/env Rscript
# ENTER DATA 
data1 <- read.table(
 header = TRUE, # First row is the header.
 # No comments within the data.
 text = '
 A B C D
 5 3 1 D1
 2 4 6 D2
 6 7 8 D3
 ')
# check data
data1

# CREATE DATA FRAME WITH NUMERIC VARIABLES ONLY
# Exclude the string variable.
data2 <- data1[, 1:3] 
# Check the data.
data2 

# AVERAGE ACROSS BOTH VARIABLES
# ROW SUMS
rowSums(data2)

# ROW MEANS
rowMeans(data2)

# CLEAN UP
# Remove all objects from workspace.
rm(list = ls())

# DATA WITH NA
# Sample data with NA
x1 <- c(1, 2, 3, NA, 5)

# "summary" still works with NA
summary(x1)
# But "mean" doesn't
mean(x1) 

# FIND NA
# Gives index number of NA
which(is.na(x1)) 
# REMOVE NA
# Removes NA from calculations
mean(x1, na.rm = TRUE) 
# REPLACE NA 1: IS.NA
# Copies data to new object
x2 <- x1 
# If item in x2 is NA, replace with 0
x2[is.na(x2)] <- 0 
# Show revised data
x2

# REPLACE NA 2: IFELSE
# Impute mean
x3 <- ifelse(is.na(x1), mean(x1, na.rm = TRUE), x1) 
# Show revised data
x3 

# CLEAN UP
# Remove all objects from workspace.
rm(list = ls())
