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

caveman_net_rewired <- rewire(caveman_net, keeping_degseq(niter = 1000))
E(caveman_net_rewired)$color <- "grey80"
V(caveman_net_rewired)$color <- "grey60"
plot(caveman_net_rewired, layout = layout.kamada.kawai(caveman_net), vertex.size = 2, vertex.label=NA)
plot(caveman_net_rewired, layout = layout.kamada.kawai(caveman_net_rewired), vertex.size = 2, vertex.label = NA)

graph.density(caveman_net_rewired)
transitivity(caveman_net_rewired)
average.path.length(caveman_net_rewired)

caveman_net_rewired <- simulate_caveman(n = 100, clique_size = 10)
avgpathlength <- average.path.length(caveman_net_rewired) # These are the first observation
clusteringcoefficient <- transitivity(caveman_net_rewired)

iter = 100
for ( i in 2:iter){
  caveman_net_rewired <- caveman_net_rewired %>% rewire(keeping_degseq(niter = 1))
  avgpathlength <- c(avgpathlength, average.path.length(caveman_net_rewired)) # We are just appending the result to a vector
  clusteringcoefficient <- c(clusteringcoefficient, transitivity(caveman_net_rewired))
}

par(mar = c(5, 4, 4, 2) + 0.1)
plot(1:100, avgpathlength, xlab = "Number of Rewirings", ylab = "Average Path Length", main = "Caveman", type = "l")
lines(1:100, clusteringcoefficient)
plot(1:100, clusteringcoefficient, xlab = "Number of Rewirings", ylab = "Clustering Coefficient", main = "Caveman", type = "l", ylim = c(0,1))

caveman_net_rewired <-  rewire(caveman_net, keeping_degseq(niter = 10))

# Measuring connectivity of networks
bridges <- function(net){
  # empty vector to store bridge names in
  bridges <- c()
  number_components <- length(decompose.graph(net)) # grab the number of components in the original raph
  for (i in 1:length(E(net))) {
    # begin a loop through all of the edges
    net_sub <- delete.edges(net, i)
    # delete the edge in question
    if(length(decompose.graph(net_sub) ) > number_components){ # if the number of components has increased
      # save this edge as a bridge
      bridges <- c(i, bridges)
    }
  }
  # return the set of bridges
  return(bridges)
}

bridges(caveman_net_rewired)

tie_range <- function(net){
  # empty vector to save ranges
  tie_ranges <- c()
  # loop through edges
  for (i in 1:length(E(net))) {
    # which nodes are incident to the edge in quetion
    incident_vertices <- ends(net, i)
    # delete the edge
    net_sub <- delete.edges(net, i)
    # evaluate the distance for the previously connected nodes
    updated_distance <- distances(net_sub, v = incident_vertices[1,1], to = incident_vertices[1,2], mode = "all")
    # save the result
    tie_ranges <- c(tie_ranges, updated_distance)
  }
  # return the resulting tie ranges
  return(tie_ranges)
}

tie_range(caveman_net_rewired)

E(caveman_net_rewired)$color <- "grey80"
V(caveman_net_rewired)$color <- "grey60"

E(caveman_net_rewired)$range <- tie_range(caveman_net_rewired)

plot(caveman_net_rewired,
    layout = layout.kamada.kawai(caveman_net_rewired),
    vertex.size = 2,
    vertex.label=NA,
    edge.width = E(caveman_net_rewired)$range/2)
