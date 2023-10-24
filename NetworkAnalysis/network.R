#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : network
# @created     : Tuesday Oct 24, 2023 18:29:41 IST
#
# @description :
######################################################################
library(igraph)

money_edgelist = read.csv("money_edgelist.csv")
print(money_edgelist)
money_edgelist <- as.matrix(money_edgelist)
print(money_edgelist)
moneyNetwork <- graph.edgelist(money_edgelist, directed=TRUE)
print(moneyNetwork)
print(V(moneyNetwork)$name)
plot(moneyNetwork)
