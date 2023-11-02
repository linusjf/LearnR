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

# Bonacich Centrality
# Perhaps marrying your daughters off to weaker families is a good way to ensure their loyalty? 
# We could evaluate this using bonacich centrality. 
# From igraph: “Interpretively, the Bonacich power measure corresponds to the notion that the power of a vertex is recursively
# defined by the sum of the power of its alters. The nature of the recursion involved is then controlled by the power exponent: positive values imply that vertices become more powerful as their alters become more powerful (as occurs in cooperative relations), while negative 
# values imply that vertices become more powerful only as their alters become weaker (as occurs in competitive or antagonistic relations).”
bonacich <- power_centrality(marriageNet, exponent = 2, rescale = T)
bonacich <- ifelse(bonacich > 0, bonacich, 0)
bonacich <- sort(bonacich, decreasing = T)
families = names(bonacich)
names(bonacich) <- c()
df <- data.frame(Family = families, Bonacich = bonacich)
print(df)
V(marriageNet)$bonacich <- power_centrality(marriageNet, exponent = 2, rescale = T)
V(marriageNet)$bonacich <- ifelse(V(marriageNet)$bonacich > 0, V(marriageNet)$bonacich, 0)

plot(marriageNet,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     vertex.size = V(marriageNet)$bonacich/max(V(marriageNet)$bonacich) * 20)

bonacich <- power_centrality(marriageNet, exponent = -2, rescale = T)
bonacich <- ifelse(bonacich < 0, 0, bonacich)
bonacich <- sort(bonacich, decreasing = T)
families = names(bonacich)
names(bonacich) <- c()
df <- data.frame(Family = families, Bonacich = bonacich)
print(df)

V(marriageNet)$bonacich <- power_centrality(marriageNet, exponent = -2, rescale = T)
V(marriageNet)$bonacich <- ifelse(V(marriageNet)$bonacich < 0, 0, V(marriageNet)$bonacich)

plot(marriageNet,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     vertex.size = V(marriageNet)$bonacich/max(V(marriageNet)$bonacich) * 20)


# pagerank
pagerank <- page_rank(marriageNet, directed = T)$vector
pagerank <- sort(pagerank, decreasing = T)
families = names(pagerank)
names(pagerank) <- c()
df <- data.frame(Family = families, PageRank = pagerank)
print(df)
V(marriageNet)$page_rank <- page_rank(marriageNet, directed = T)$vector

plot(marriageNet,
     vertex.label.cex = .6, 
     vertex.label.color = "black", 
     vertex.size = V(marriageNet)$page_rank/max(V(marriageNet)$page_rank) * 20)

# correlation between centrality measures
# extract all the vertex attributes
all_atts <- lapply(list.vertex.attributes(marriageNet),function(x) get.vertex.attribute(marriageNet,x))
# bind them into a matrix
all_atts <- do.call("cbind", all_atts)
# add column nams
colnames(all_atts) <- list.vertex.attributes(marriageNet)
# drop the family variable
all_atts <- data.frame(all_atts[,2:ncol(all_atts)])
# convert all to numeric
all_atts <- sapply(all_atts, as.numeric)
# produce a correlation matrix
cormat <- cor(all_atts)
# melt it using reshape to function melt() to prepare it for ggplot which requires long form data
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_distiller(palette = "Spectral", direction=-2) +
  xlab("") +
  ylab("")

# degree distribution
# fitting a degree distribution on the log-log scale
alter_hist = degree(marriageNet)
alter_hist = table(alter_hist)
vals = as.numeric(names(alter_hist))
vals = vals[2:length(vals)]
alter_hist = alter_hist[2:length(alter_hist)]
df = data.frame(Vals = log(vals), Hist = log(as.numeric(alter_hist)), stringsAsFactors = F)
# plot log-log degree distribution
plot(Hist ~ Vals, data = df)
# regression line
abline(lm(Hist ~ Vals, data = df))

# degrees of your friends
neighbor_degrees <- knn(marriageNet)$knn
degrees <- degree(marriageNet)

mean(neighbor_degrees, na.rm = T)
mean(degrees)
# plot neighbor degrees vs. ego degress
hist(neighbor_degrees)
hist(degrees)

degcent <- centralization.degree(marriageNet)$centralization
degcent
centralization.betweenness(marriageNet)$centralization
centralization.evcent(marriageNet)$centralization
centralization.closeness(marriageNet)$centralization

N <- vcount(marriageNet)
degcent <- centralization.degree(marriageNet)$centralization

centralizations = c()
powers <- seq(from = 0.1, to = 3, by = 0.1)
for(e in powers){
  net <- barabasi.game(N, directed = F, power=e)
  centralizations <- c(centralizations, centralization.degree(net)$centralization)
}

power_df <- data.frame(Centralization = centralizations, Power = powers)
power_df
ggplot(power_df, aes(x = Power, y = Centralization)) + 
  geom_point() + 
  geom_hline(yintercept = degcent, linetype="dashed", color = "red") +
  theme_bw()

reach_n =function(x, n = 2){
  vert_cnt = vcount(x)
  r=vector(length=vert_cnt)
  for (i in 1:vert_cnt){
    neighb =neighborhood(x, n, nodes=i)
    ni=unlist(neighb)
    len=length(ni)
    r[i]=len/vert_cnt
  }
  return(r)
}
two_reach = reach_n(marriageNet, 2)
plot(marriageNet, vertex.size = two_reach * 10, vertex.label.cex = .4, vertex.label.color = "black", vertex.color = "tomato")
three_reach = reach_n(marriageNet, 3)
plot(marriageNet, vertex.size = three_reach * 10, vertex.label.cex = .4, vertex.label.color = "black", vertex.color = "tomato")
four_reach = reach_n(marriageNet, 4)
plot(marriageNet, vertex.size = four_reach * 10, vertex.label.cex = .4, vertex.label.color = "black", vertex.color = "tomato")
five_reach = reach_n(marriageNet, 5)
plot(marriageNet, vertex.size = five_reach * 10, vertex.label.cex = .4, vertex.label.color = "black", vertex.color = "tomato")

distance_weighted_reach=function(x){
  # create matrix of geodesic distances
  distances = shortest.paths(x)
  # replace the diagonal with 1s
  diag(distances) = 1 
  # take the reciprocal of distances
  weights = 1/distances 
  # sum for each node (row)
  wr = apply(weights,1,sum)
  return (wr) 
}

dw_reach = distance_weighted_reach(marriageNet) 
print(dw_reach)
dw_reach = dw_reach/max(dw_reach)
print(dw_reach)
plot(marriageNet, vertex.size = dw_reach * 10, vertex.label.cex = .4, vertex.label.color = "black", vertex.color = "tomato")
