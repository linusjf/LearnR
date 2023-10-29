#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : comm59
# @created     : Saturday Oct 28, 2023 21:24:42 IST
#
# @description :
######################################################################
# First read in igraph
library(igraph)
# read in the edge list from our github
el <- read.table("comm59.csv", header = T)
print(str(el))
# Read in attributes from our github
attributes <- read.table("comm59_att.csv", header = T)
print(str(attributes))
# add an ID column
attributes$ID <- 1:nrow(attributes)

# Indexing data so that you only put in certain columns
# We will ignore the ranking variable for now.
el_no_weight <- el[,1:2]
# igraph requires a matrix
el_no_weight <- as.matrix(el_no_weight)

# convert ids to characters so they are preserved as names
el_no_weight[,1] <- as.character(el_no_weight[,1])
el_no_weight[,2] <- as.character(el_no_weight[,2])

# Graph the network
net59 <- graph.edgelist(el_no_weight, directed = T)

# Finally, add attributes
# First link vertex names to their place in the attribute dataset
linked_ids <- match(V(net59)$name, attributes$ID)

# Then we can use that to assign a variable to each user in the network
V(net59)$race <- attributes$race[linked_ids]
V(net59)$sex <- attributes$sex[linked_ids]
V(net59)$grade <- attributes$grade[linked_ids]
V(net59)$school <- attributes$school[linked_ids]
# Great!
print(net59)
