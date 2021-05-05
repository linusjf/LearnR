#!/usr/bin/env Rscript

# LOAD DATA Load datasets package
require("datasets")
# Tabular data in 3 dimensions; N = 4526.
str(UCBAdmissions)
# Prints six tables; one for each department.
print(UCBAdmissions)

# SHOW MARGINAL FREQUENCIES Admit
margin.table(UCBAdmissions, 1)
# Gender
margin.table(UCBAdmissions, 2)
# Dept
margin.table(UCBAdmissions, 3)
# Total
margin.table(UCBAdmissions)

# SAVE MARGINALS
admit.dept <- margin.table(UCBAdmissions, 3)
# Creates table for dept

# Describe the structure of the data table
str(admit.dept)
# Makes a default barplot of the frequencies
barplot(admit.dept)
# Show frequencies
admit.dept
# Show as proportions
prop.table(admit.dept)
# Show as proportions w/2 digits
round(prop.table(admit.dept), 2)
# Percentages w/o decimals
round(prop.table(admit.dept), 2) * 100

# RESTRUCTURE DATA Coerces to data frame
admit1 <- as.data.frame.table(UCBAdmissions)
# This repeats each row by Freq
admit2 <- lapply(admit1, function(x) rep(x, admit1$Freq))
# Converts from list back to data frame.
admit3 <- as.data.frame(admit2)
# Removes fifth column that has frequencies.
admit4 <- admit3[, -4]
# admit4 is the final data set, ready for analysis by case.
summary(admit4)
