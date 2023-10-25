#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : attributesna
# @created     : Wednesday Oct 25, 2023 11:11:30 IST
#
# @description :
######################################################################
library(igraph)
attributes <- read.csv("hoffmans.csv", stringsAsFactors = F)
print(head(attributes))
# Load in the edge list again
money_edgelist <- read.csv("money_edgelist.csv", stringsAsFactors = F)
# Put them both in the network.
moneyNetwork <- graph_from_data_frame(money_edgelist, directed = T, vertices = attributes)
print(moneyNetwork)
print(V(moneyNetwork)$name)
print(V(moneyNetwork)$Age)
print(V(moneyNetwork)$Gender)
print(V(moneyNetwork)$Role)
