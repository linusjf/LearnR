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
V(moneyNetwork)$color <- ifelse(V(moneyNetwork)$Gender == "Male", "dodgerblue3","seagreen")
plot(moneyNetwork, vertex.size = 10, vertex.frame.color = "black", vertex.label.cex = .7, vertex.label = NA, edge.curved = .1, edge.arrow.size = .3)
V(moneyNetwork)$color <- NA
V(moneyNetwork)$color <- ifelse(V(moneyNetwork)$Role == "Father", "burlywood1","tomato")
V(moneyNetwork)$color <- ifelse(V(moneyNetwork)$Role == "Mother", "seagreen", V(moneyNetwork)$color)
V(moneyNetwork)$color <- ifelse(V(moneyNetwork)$Role == "Son", "grey70", V(moneyNetwork)$color)

plot(moneyNetwork,vertex.size = 10, vertex.label.cex = .7, vertex.label = NA, edge.curved = .1, vertex.frame.color = "black", edge.arrow.size = .3, edge.width = .7, edge.color = "grey30")
V(moneyNetwork)$size = V(moneyNetwork)$Age/5
plot(moneyNetwork, vertex.label.cex = .7, vertex.label = NA, edge.curved = .1, vertex.frame.color = "black", edge.arrow.size = .3, edge.width = .7, edge.color = "grey30")
