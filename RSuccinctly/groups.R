#!/usr/bin/env Rscript
# ENTER DATA
# Hits in millions for each word on Google
groups <- c(
  rep("blue", 3990),
  rep("red", 4140),
  rep("orange", 1890),
  rep("green", 3770),
  rep("purple", 855)
)

# CREATE FREQUENCY TABLES
# Creates frequency table
groups.t1 <- table(groups)
# Print table
print(groups.t1)
# MODIFY FREQUENCY TABLES
# Sorts by frequency
groups.t2 <- sort(groups.t1, decreasing = TRUE)
# Prints table
print(groups.t2)

# PROPORTIONS AND PERCENTAGES
# Gives proportions of the total.
prop <- prop.table(groups.t2)
prop.round <- round(prop, 2)
prop.percent <- prop.round * 100

print(prop)
print(prop.round)
print(prop.percent)

rm(list = ls())
