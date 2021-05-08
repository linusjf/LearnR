#!/usr/bin/env Rscript
# LOAD DATA
# Load the datasets package.
library("Hmisc")
library("datasets")
data(swiss)
# CORRELATION MATRIX
# Rounded to 2 decimals
print(round(cor(swiss), 2))

# INFERENTIAL TEST FOR SINGLE CORRELATION
cor.test(swiss$Fertility, swiss$Education)

# COERCE DATA FRAME TO MATRIX
# GET R & P MATRICES
rcorr(as.matrix(swiss))

# CLEAN UP
# Unload the package.
detach("package:datasets", unload=TRUE) 
# Unload the package.
detach("package:Hmisc", unload=TRUE) 
# Remove the objects.
rm(list = ls()) 
