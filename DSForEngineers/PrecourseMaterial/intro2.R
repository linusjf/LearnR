#!/usr/bin/env Rscript

A <- 14
B <- 23
print(c(A, B))
print(ls())
rm(A)
print(ls())
rm(list = ls())
print(ls())

A <- 14
B <- 23
# to save a single variable ‘a’
save(B, file = "sess1.RData")
# to save a full workspace with specified file name
save(list = ls(all.names = TRUE), file = "sess2.RData")
# short cut function to save whole workspace
save.image()
# to load saved workspace
load(file = "sess2.RData")
print(ls())
