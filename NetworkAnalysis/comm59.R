#!/usr/bin/env Rscript
######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : comm59
# @created     : Saturday Oct 28, 2023 21:24:42 IST
#
# @description :
######################################################################
library(ggplot2)
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
print(gorder(net59))
print(length(attributes$race[linked_ids]))
V(net59)$race <- attributes$race[linked_ids]
V(net59)$sex <- attributes$sex[linked_ids]
V(net59)$grade <- attributes$grade[linked_ids]
V(net59)$school <- attributes$school[linked_ids]
print(net59)
print("Graph density: ")
print(graph.density(net59))
print("Graph reciprocity: ")
print(reciprocity(net59))
net59_n <- gorder(net59)
net59_density <- graph.density(net59)
# where n is the number of nodes, p.or.m is the probability of drawing an edge, directed is whether the network is directed or not
random_graph <- erdos.renyi.game(n = net59_n, p.or.m = net59_density, directed = TRUE)
plot(random_graph,
    vertex.size = 2,
    vertex.label = NA,
    edge.curved = .1,
    vertex.color = "tomato",
    edge.arrow.size = .1,
    edge.width = .5,
    edge.color = "grey60")
print("Random Graph reciprocity: ")
print(reciprocity(random_graph))
print(triad.census(net59))
print(triad.census(random_graph))

# this creates a list with 100 spaces to store things.  We will store each result here.
trial <- vector("list", 100)

for ( i in 1:length(trial) ){
  random_graph <- erdos.renyi.game(n = net59_n, p.or.m = net59_density, directed = TRUE)
  # We assign to the ith space the result. So for the first iteration, it will assign the result to the first space in the list
  trial[[i]] <- triad.census(random_graph)
}

# We can use the do.call and "rbind" functions together to combine all of the results into a matrix, where each row is one of our trials
trial_df <- do.call("rbind", trial)

colnames(trial_df) <- c("003", "012", "102", "021D", "021U", "021C", "111D", "111U", "030T", "030C", "201", "120D", "120U", "120C", "210", "300") # It is worth naming the columns too.

# add in the observed results
trial_df_w_observed <- rbind(trial_df, as.numeric(triad.census(net59)))

# First, standardize all of the columns by dividing each of their values by the largest value in that column, so that each will be on a similar scale (0 to 1), we can visualize them meaningfully
trial_df_w_observed <- as.data.frame(trial_df_w_observed)

trial_df_w_observed[,1:ncol(trial_df_w_observed)] <- sapply(trial_df_w_observed[,1:length(trial_df_w_observed)], function(x) x/max(x))

# Then split the observed from the simulation results
trial_df <- as.data.frame(trial_df_w_observed[1:100,])
observed <- as.numeric(trial_df_w_observed[101,])

# Summarize the simulation results and add the observed data set back in for comparison
summarized_stats <- data.frame(TriadType = colnames(trial_df),
                                Means = sapply(trial_df, mean),
                                LowerCI = sapply(trial_df, function(x) quantile(x, 0.05)),
                                UpperCI = sapply(trial_df, function(x) quantile(x, 0.95)),
                                Observed = observed)

print(summarized_stats)

ggplot(summarized_stats) +
    geom_point(aes(x=TriadType, y=Means, colour=TriadType)) +
    geom_errorbar(aes(x = TriadType, ymin=LowerCI, ymax=UpperCI, colour=TriadType), width=.1) +
    geom_point(aes(x=TriadType, y=Observed, colour="Observed")) +
    coord_flip()
