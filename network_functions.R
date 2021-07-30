# Network setup function
library(igraph)
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

f.graph = function(grid_size, graph_type, Ndist, Ncounty) {
  # graph_type = 0,1,2,3 for real map, sq, hex, NCdummydata respectively.
  if (graph_type == 0) {
    nodes = 26
  } else {
    nodes = grid_size^2 # grid_size is 51 for the NC data
  }
  
  if (grid == 3) {
    load("/Users/laura/Documents/MATH5871_Dissertation/Programming/Rcode/data_cleaning/NCData.RData")
    P.data = NCData[order(NCData$district),]
    set.seed(1)
    remove_index = sample(1:2692,91)
    P.data = P.data[-remove_index,]
    P.data$area = rep(2.6, dim(P.data)[1])
    row.names(P.data) = NULL
    P.data$name = 1:dim(P.data)[1]
    P.data = P.data[,c(dim(P.data)[2],1:(dim(P.data)[2]-1))]
    
  } else {
    district0 = rep(Ndist,nodes)
    district0 = replace(district0, 1:(Ndist*floor(nodes/Ndist)),
                        rep(1:Ndist, each=(nodes/Ndist)))
    county = rep(Ncounty,nodes)
    county = replace(county, 1:(Ncounty*floor(nodes/Ncounty)),
                     rep(1:Ncounty, each=(nodes/Ncounty)))
    P.data = data.frame(unitID = 1:nodes, population = rep(1, nodes),
                        blue = sample(1:5,nodes,replace = T), 
                        red = sample(1:5,nodes,replace = T),
                        district = district0, county=county, area=rep(2.6,nodes))
  }

  if (graph_type == 0) {
    realmap = read.csv("/Users/laura/Documents/MATH5871_Dissertation/Programming/Data/UK/dummywardadjm.csv",
                       header=T)
    realmap=as.matrix(realmap[,2:27])
    adjm = realmap
  } else if (graph_type == 1) {
    adjm = f.adjm.sq(grid_size)
  } else {
    adjm = f.adjm.hex(grid_size)
  }
  E.data = f.edges(adjm)
  class(E.data)
  E.data$weight = rep(1, dim(E.data)[1])
  # I now have two data frames - one containing precinct data and one containing 
  # edge connections and edge lengths.
  # Make the graph object
  g = graph_from_data_frame(E.data, directed = FALSE, 
                            vertices=P.data)
}

# Function to determine which vertices are on the perimeter - add a perimeter 
# node with id no. nodes+1 and add the external edges.
f.perimeter = function(g,nodes) {
  # Perimeter nodes are identified by creating a subgraph of the neighbors and 
  # checking whether the degree of any of theses is 0 or 1. This indicates 
  # no cyclical path around vertex.
  for (i in 1:length(V(g))) {
    gneighbors = induced_subgraph(g,neighbors(g,i))
    deg = degree(gneighbors)
    V(g)$external[i] = ifelse(1 %in% deg | 0 %in% deg, 1, 0)
  }
  #print(V(g)$external)
  # Before you add the edges you need to add perimeter vertex
  g = g + vertex(nodes+1,population=0,votes_blue=0,
                 votes_red=0,district=0,size=0,external=0)
  # Add edge information
  c=cbind(rep(nodes+1, sum(V(g)$external)),which(V(g)$external==1))
  c=t(c)
  c=as.vector(c)
  g = g + edges(c, weight=1)
  #print(get.edgelist(g))
  g
}


