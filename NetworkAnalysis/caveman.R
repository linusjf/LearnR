#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : caveman
# @created     : Friday Nov 03, 2023 20:19:17 IST
#
# @description :
######################################################################
library(conflicted)
library(igraph)

simulate_caveman <- function(n = 25, clique_size = 5){
  # Groups are all the same size, so I check whether N is divisible by the size of groups
  if ( ((n%/%clique_size) * clique_size) != n){
    stop("n is not evenly divisible by clique_size")
  }

  # this determines the number of groups
  groups = n/clique_size

  el <- data.frame(PersonA = 1:n, Group = NA) # I create a dataframe which has people and the groups they are in
  # I treat it like a person to group edgelist

  group_vector = c()
  for (i in 1:groups){
    group_vector <- c(group_vector, rep(i, clique_size))
  }
  el$Group <- group_vector

  # I use the table function to turn the person to group edgelist into an incidence matrix
  inc <- table(el)
  # And I use matrix multiplication with the transpose to turn the person to group incidence matrix
  # into a person to person adjacency matrix
  adj <- inc %*% t(inc)

  diag(adj) <- 0
  # I graph this matrix
  g <- graph.adjacency(adj, mode = "undirected")

  # I determine the points of connection using a sequence function
  group_connect <- seq(from = 1, to = n, by = clique_size)

  for( i in 1:(length(group_connect)-1)){
    p1 <- group_connect[i] + 1
    p2 <- group_connect[i+1]
    # And I connect the points of connection using add.edges
    g <- add.edges(g, c(p1,p2))
  }
    # finally I connect the ends of the structure so that it forms a circle
    g <- add.edges(g, c(group_connect[1],(group_connect[groups]+1)))
    return(g)
}

caveman_net <- simulate_caveman(n = 100, clique_size = 5)
print_all(caveman_net)
par(mar = c(2,2,2,2))
plot(caveman_net, layout = layout.kamada.kawai(caveman_net), vertex.size = 2, vertex.label = NA, vertex.color = "grey80")
print(graph.density(caveman_net))
# transitivity() measures clustering coefficient, which essentially says, how clustered is the network overall
print(transitivity(caveman_net))
average.path.length(caveman_net)

nodes_diameter<-get.diameter(caveman_net)
edges_incident <- get.edge.ids(caveman_net, nodes_diameter)

# Set default color for nodes
V(caveman_net)$color<-"grey60"
# Set the nodes on the diameter to be green
V(caveman_net)[nodes_diameter]$color<-"green"

# Set default edge color
E(caveman_net)$color<-"grey70"
# Set the edges on the diameter to be green
E(caveman_net)[edges_incident]$color<-"green"

plot(caveman_net, layout = layout.kamada.kawai(caveman_net), vertex.size = 2, vertex.label = NA)
