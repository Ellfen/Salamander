# This script provides a function for creating an nxn grid by first creating 
# the lower triangle of an adjacency matrix (adjacency matrices are symmetric)
# and then coercing the square adjacency matrix into a 2 column matrix of edge
# information. This edge information can then be used in conjunction with a 
# dataframe of vertice/node information to construct the grid. In applications
# the dataframe of vertices/nodes will contain additional information on the
# population of the precint the node represents and the proportion of votes 
# going to each party.
# WILL ADJMS ALWAYS BE SQUARE?? YES THE MATRIX WILL BE SQUARE BUT THAT DOES NOT
# MEAN THE MAP ITSELF WILL BE SQUARE.

library(igraph)
library(tictoc)

# A function for constructing the lower triangle of an adjacency matrix for an 
# nxn grid. Note that adjacency matrices are symmetric.
f.adjm.sq = function(n) {
  nodes = n^2
  adjm = matrix(0, nodes, nodes)
  diag(adjm[-1,]) = c(rep(c(rep(1,n-1),0),n-1),rep(1,n-1))
  diag(adjm[-c(1:n),]) = rep(1,nodes-n)
  adjm
}

f.adjm.hex = function(n) {
  nodes = n^2
  adjm = matrix(0, nodes, nodes)
  diag(adjm[-1,]) = c(rep(c(rep(1,n-1),0),n-1),rep(1,n-1))
  diag(adjm[-c(1:(n+1)),]) = c(rep(c(rep(1,n-1),0),n-2),rep(1,n-1))
  diag(adjm[-c(1:n),]) = rep(1,nodes-n)
  adjm
}

# A function for coercing adjacency matrix into a 2 column matrix
f.edges = function(adjm) {
  nodes = dim(adjm)[1]
  edges = c(2,1)
  for (i in 3:nodes) {
    v2=which(adjm[i,]==1)
    v1=rep(i,length(v2))
    X = cbind(v1, v2); X
    edges = rbind(edges, X)
  }
  edges = as.data.frame(edges)
  row.names(edges) = NULL
  edges
}
# Note that the number of rows in edges is sum(adjm)

# # To test f.adjm
# adjm = f.adjm(3); adjm
# g = graph_from_adjacency_matrix(adjm, mode="lower")
# plot(g)
# 
# # To test f.edges
# adjm = f.adjm(4)
# edges = f.edges(adjm)
# g = graph_from_data_frame(edges, directed = FALSE, vertices = 1:16)
# plot(g, vertex.size=5, vertex.label=NA)
# sum(adjm)
# dim(edges)[1]
# 
# # To see how fast my functions are with large grids
# n=5
# tic()
# adjm = f.adjm(n)
# toc()
# 
# tic()
# edges = f.edges(adjm)
# toc()
# 
# g = graph_from_data_frame(edges, directed = FALSE, vertices = 1:n^2)
# neighbors(g,12)
# 
# # To plot - this is slow and works for a 20x20 grid but not a 30x30 grid. I need
# # a better way of plotting.
# png("grid.png")
# tic()
# plot(g, vertex.size=5, vertex.label=NA)
# toc()
# dev.off(); graphics.off()
# 
