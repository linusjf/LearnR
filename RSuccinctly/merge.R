#!/usr/bin/env Rscript
# LOAD DATA
# Load the data sets package.
library("datasets")
# DISPLAY DATA
# Display the first three rows, all variables.
longley[1:3, ]

# SPLIT & EXPORT DATA
# First 14 cases, first 6 variables.
a1 <- longley[1:14, 1:6]
# First 14 cases, last 2 variables.
a2 <- longley[1:14, 6:7]
# Last 2 cases, all variables.
b <- longley[15:16, ]

write.table(a1, "longley.a1.txt", sep = "\t")
write.table(a2, "longley.a2.txt", sep = "\t")
write.table(b, "longley.b.txt", sep = "\t")
# Clear out everything to start fresh
rm(list = ls())


# IMPORT & COMBINE FIRST TWO DATA SETS
# Add columns for same cases.
a1t <- read.table("longley.a1.txt", sep = "\t")
a2t <- read.table("longley.a2.txt", sep = "\t")
# Take early years (a1t) and add columns (a2t).
# Must specify the variable to match cases ("Year" in this case).
# Merge two data frames
a.1.2 <- merge(a1t, a2t, by = "Year")
# Check results for the first three cases.
a.1.2[1:3, ]

# IMPORT & COMBINE LAST DATA SET
# Add two more cases at the bottom.
b <- read.table("longley.b.txt", sep = "\t")
# "Row Bind"
all.data <- rbind(a.1.2, b)
# Check last four rows, all variables.
all.data[12:16, ]
# CLEAN DATA
# Reset row names.
row.names(all.data) <- NULL
# Check last four rows, all variables.
all.data[13:16, ]

# CLEAN UP
# Unloads data sets package.
detach("package:datasets", unload = TRUE)
# Remove all objects from workspace.
rm(list = ls())
