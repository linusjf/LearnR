#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : egonetworks
# @created     : Wednesday Oct 25, 2023 14:15:42 IST
#
# @description :
######################################################################
library(igraph)
gss <- read.csv("gss_local_nets.csv", stringsAsFactors = TRUE)
print(head(gss))

ties <- gss[,grepl("close", colnames(gss))]
print(head(ties))

debug_ego_nets <- function() {
  mat = matrix(nrow = 5, ncol = 5)
  print(mat)
  print(ties[3,])
  mat[lower.tri(mat)] <- as.numeric(ties[3,])
  print(mat)
  print(t(mat))
  print(upper.tri(mat))
  print(t(mat)[upper.tri(mat)])
  mat[upper.tri(mat)] = t(mat)[upper.tri(mat)]
  print(mat)
  na_vals <- is.na(mat)
  print(na_vals)
  print(rowSums(na_vals))
  print(nrow(mat))
  non_missing_rows <- rowSums(na_vals) < nrow(mat)
  print(non_missing_rows)
  mat <- mat[non_missing_rows,non_missing_rows]
  print(mat)
  diag(mat) <- 0
  print(mat)
  ego_net <- graph.adjacency(mat, mode = "undirected", weighted = T)
  plot(ego_net, vertex.size = 30, vertex.label.color = "black", vertex.label.cex = 1, main="Respondent 3")
}

make_ego_nets <- function(tie){
  # make the matrix
  mat = matrix(nrow = 5, ncol = 5)
  # assign the tie values to the lower triangle
  mat[lower.tri(mat)] <- as.numeric(tie)
  # symmetrize
  mat[upper.tri(mat)] = t(mat)[upper.tri(mat)]
  # identify missing values
  na_vals <- is.na(mat)
  # identify rows where all values are missing
  non_missing_rows <- rowSums(na_vals) < nrow(mat)

  # if any rows
  if(sum(!non_missing_rows) > 0){
    mat <- mat[non_missing_rows,non_missing_rows]
  }
  diag(mat) <- 0
  ego_net <- graph.adjacency(mat, mode = "undirected", weighted = T)
  return(ego_net)
}

debug_ego_nets()

ego_nets <- lapply(1:nrow(ties),
                    FUN=function(x) make_ego_nets(ties[x,]))

head(ego_nets)

random_ego_net <- ego_nets[[1021]]
print(random_ego_net)
plot(random_ego_net, main="Respondent 1021")
