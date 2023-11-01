#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : centrality
# @created     : Tuesday Oct 31, 2023 11:21:31 IST
#
# @description :
######################################################################
library(conflicted)
library(igraph)
library(tidyverse)
library(reshape2)
conflict_scout()
# prepare the marriage adjacency matrix
florentine_edj <- read.csv("florentine_marriage_edgelist.csv")
print(str(florentine_edj))
florentine_edj <- florentine_edj[,2:3]

# prepare the attributes file
florentine_attributes <- read.csv("florentine_attributes.csv")
print(str(florentine_attributes))

# graph the marriage network
marriageNet <- graph.edgelist(as.matrix(florentine_edj), directed = T)
# add wealth and priorates attributes to family vertices
V(marriageNet)$Wealth <- florentine_attributes$Gwealth[match(V(marriageNet)$name, florentine_attributes$Family)]

# Gross wealth (Florins), for 87 (92) families
# simple mean imputation of wealth (alternatively, we might think that those with NA were too poor to show up in historical records?)
# in other words, atrribute mean wealth to na families
V(marriageNet)$Wealth <- ifelse(is.na(V(marriageNet)$Wealth), mean(V(marriageNet)$Wealth, na.rm = T), V(marriageNet)$Wealth)

# Number of Priors, The Priorate (or city council), first created in 1282, was Florence's governing body. Count of how many seats a family had on that city council from 1282-1344
# measure of the aggregate political influence of the family over a long period of time
V(marriageNet)$Priorates <- florentine_attributes$Npriors[match(V(marriageNet)$name, florentine_attributes$Family)]

plot(marriageNet, vertex.size = 8, vertex.label.cex = .4, vertex.label.color = "black", vertex.color = "tomato", edge.arrow.size = 0.4)

print(str(degree(marriageNet)))
degrees <- sort(degree(marriageNet), decreasing = T)
families = names(degrees)
names(degrees) <- c()
df <- data.frame(Family = families, Degree = degrees)
print(df)
# assignment
V(marriageNet)$degree <- degree(marriageNet)
# sized by degree
plot(marriageNet, vertex.label.cex = .6, vertex.label.color = "black", vertex.size = V(marriageNet)$degree, vertex.label.cex = .2)
plot(marriageNet,
      vertex.label.cex = .6,
      vertex.label.color = "black",
      vertex.size = V(marriageNet)$degree*3)

btws = betweenness(marriageNet, directed = FALSE)
btws <- sort(btws, decreasing = T)
families = names(btws)
names(btws) <- c()
df <- data.frame(Family = families, Betweenness = btws)
print(df)
# assignment
V(marriageNet)$betweenness <- betweenness(marriageNet, directed = F)
# sized by betweenness
plot(marriageNet,
      vertex.label.cex = .6,
      vertex.label.color = "black",
      vertex.size = V(marriageNet)$betweenness)
# normalize betweenness and scale by 20 (some scalar)
plot(marriageNet,
      vertex.label.cex = .6,
      vertex.label.color = "black",
      vertex.size = V(marriageNet)$betweenness/max(V(marriageNet)$betweenness) * 20)

clseness = closeness(marriageNet, normalized = F)
clseness <- sort(clseness, decreasing = T)
families = names(clseness)
names(clseness) <- c()
df <- data.frame(Family = families, Closeness = clseness)
print(df)
# assignment
V(marriageNet)$closeness <- closeness(marriageNet, normalized = F)
plot(marriageNet,
      vertex.label.cex = .6,
      vertex.label.color = "black",
      vertex.size = V(marriageNet)$closeness/max(V(marriageNet)$closeness) * 20)

# Eigenvector centrality takes into account alters’ power. 
# It is calculated a little bit differently in igraph. It produces a list object
# and we need to extract only the vector of centrality values.
power = evcent(marriageNet)$vector
power <- sort(power, decreasing = T)
families = names(power)
names(power) <- c()
df <- data.frame(Family = families, Power = power)
print(df)
V(marriageNet)$eigenvector <- evcent(marriageNet)$vector

plot(marriageNet,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     vertex.size = V(marriageNet)$eigenvector/max(V(marriageNet)$eigenvector) * 20)

