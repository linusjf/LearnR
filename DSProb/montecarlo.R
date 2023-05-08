#!/usr/bin/env Rscript
beads <- rep(c("red", "blue"), times = c(2, 3)) # create an urn with 2 red, 3 blue
# view beads object
print(beads)
# sample 1 bead at random
sample(beads, 1)

# number of times to draw 1 bead
B <- 10000
events <- replicate(B, sample(beads, 1)) # draw 1 bead, B times
# make a table of outcome counts
tab <- table(events)
# view count table
print(tab)
# view table
prop.table(tab)
