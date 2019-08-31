#!/usr/bin/env Rscript
if(!exists("findwords", mode="function")) 
  source("findwords.R")

wrdlist <- findwords("testconcorda.txt")
sorted <- alphawl(wrdlist)
print(sorted)
frqwl <- freqwl(wrdlist)
print(frqwl)
