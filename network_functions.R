# Network setup function
library(igraph)
library(redist)
library(sf)
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
  edges = c(NA,NA)
  for (i in 1:nodes) {
    v2=which(adjm[i,]==1)
    v1=rep(i,length(v2))
    X = cbind(v1, v2)
    edges = rbind(edges, X)
  }
  edges = as.data.frame(edges[-1,])
  row.names(edges) = NULL
  edges
}
# old code
# f.edges = function(adjm) {
#   nodes = dim(adjm)[1]
#   edges = c(2,1)
#   for (i in 3:nodes) {
#     v2=which(adjm[i,]==1)
#     v1=rep(i,length(v2))
#     X = cbind(v1, v2)
#     edges = rbind(edges, X)
#   }
#   edges = as.data.frame(edges)
#   row.names(edges) = NULL
#   edges
# }

f.graph = function(grid_size, graph_type, Ndist, Ncounty) {
  # graph_type = 0,1,2,3,4,5 for real map, sq, hex, NCdummy, NCreal, NCsub.
  if (graph_type == 0) {
    nodes = 26
  } else if (graph_type == 4) {
    nodes = 2692
  } else if (graph_type == 5) {
    nodes = 1060
  } else {
    nodes = grid_size^2 # grid_size is 51 for the NCdummydata
  }
  
  if (graph_type == 3) {
    load("/Users/laura/Documents/MATH5871_Dissertation/Programming/Rcode/data_cleaning/NCData.RData")
    P.data = NCData[order(NCData$district),]
    set.seed(1)
    remove_index = sample(1:2692,91)
    P.data = P.data[-remove_index,]
    P.data$area = rep(2.6, dim(P.data)[1])
    row.names(P.data) = NULL
    P.data$name = 1:dim(P.data)[1]
    P.data = P.data[,c(dim(P.data)[2],1:(dim(P.data)[2]-1))]
  } else if (graph_type == 4) {
    NCshp = st_read("data_cleaning/NCDataGeom.shp")
    P.data = as.data.frame(NCshp)
    P.data = P.data[,-dim(P.data)[2]]
  } else if (graph_type == 5) {
    NCshp = st_read("data_cleaning/NCDataSub.shp")
    P.data = as.data.frame(NCshp)
    P.data = P.data[,-dim(P.data)[2]]
  } else {
    district0 = rep(Ndist,nodes)
    district0 = replace(district0, 1:(Ndist*floor(nodes/Ndist)),
                        rep(1:Ndist, each=(nodes/Ndist)))
    county = rep(Ncounty,nodes)
    county = replace(county, 1:(Ncounty*floor(nodes/Ncounty)),
                     rep(1:Ncounty, each=(nodes/Ncounty)))
    P.data = data.frame(name = 1:nodes, unitID = 1:nodes, county=county,
                        blue = c(2,1,2,5,3,2,5,1,3,2,4,1,2,2,1,1),#sample(1:5,nodes,replace = T),
                        red = c(4,4,5,3,3,4,2,5,5,5,2,4,2,2,1,2),#sample(1:5,nodes,replace = T),
                        population = rep(10, nodes),
                        district = district0,  area=rep(2.6,nodes),
                        centroidx = c(6,7.5,9,10.5,4.5,6,7.5,9,3,4.5,6,7.5,1.5,3,4.5,6), 
                        centroidy = c(0,0.87,1.73,2.6,0.87,1.73,2.6,3.46,
                                      1.73,2.6,3.46,4.33,2.6,3.46,4.33,5.17))
                                      #c(2,3,4,5,3,4,5,6,4,5,6,7,5,6,7,8))
  }

  if (graph_type == 0) {
    realmap = read.csv("/Users/laura/Documents/MATH5871_Dissertation/Programming/Data/UK/dummywardadjm.csv",
                       header=T)
    realmap=as.matrix(realmap[,2:27])
    adjm = realmap
  } else if (graph_type == 1) {
    adjm = f.adjm.sq(grid_size)
  } else if (graph_type == 4 | graph_type == 5) {
    adjlist = redist.adjacency(NCshp)
    adjm = matrix(0, nrow=length(adjlist),ncol=length(adjlist))
    for (i in 1:length(adjlist)) {
      unlist = adjlist[[i]]+1
      unlist = unlist[which(unlist>i)]
      for (j in 1:length(unlist)) {
        adjm[i,unlist[j]] = 1
      }
    }
  } else {
    adjm = f.adjm.hex(grid_size)
  }
  E.data = f.edges(adjm)
  E.data$weight = rep(1, dim(E.data)[1])
                  #c(1.76,1.76,1.76,1.76,2,1.76,1.76,2,1.76,1.76,2,1.76,1.76,
  #                   1.76,2,1.76,1.76,2,1.76,1.76,2,1.76,1.76,1.76,2,1.76,1.76,
  #                   2,1.76,1.76,2,1.76,1.76)
  # I now have two data frames - one containing precinct data and one containing 
  # edge connections and edge lengths.
  # Make the graph object
  g = graph_from_data_frame(E.data, directed = FALSE, 
                            vertices=P.data)
  
}

f.graph40 = function() {
  # only accepts grid type 5
  nodes = 1060
  #NCshp = st_read("data_cleaning/NCData40KSteps.shp")
  NCshp = st_read("data_cleaning/Anneal_30K_500_2_2_10KB.shp")
  P.data = as.data.frame(NCshp)
  P.data = P.data[,-dim(P.data)[2]]
  adjlist = redist.adjacency(NCshp)
  adjm = matrix(0, nrow=length(adjlist),ncol=length(adjlist))
  for (i in 1:length(adjlist)) {
    unlist = adjlist[[i]]+1
    unlist = unlist[which(unlist>i)]
    for (j in 1:length(unlist)) {
      adjm[i,unlist[j]] = 1
    }
  }
  E.data = f.edges(adjm)
  E.data$weight = rep(1, dim(E.data)[1])
  g = graph_from_data_frame(E.data, directed = FALSE, 
                            vertices=P.data)
}

# Function to determine which vertices are on the perimeter - add a perimeter 
# node with id no. nodes+1 and add the external edges.
# f.perimeter = function(g,nodes) {
#   # Perimeter nodes are identified by creating a subgraph of the neighbors and 
#   # checking whether the degree of any of theses is 0 or 1. This indicates 
#   # no cyclical path around vertex.
#   for (i in 1:length(V(g))) {
#     gneighbors = induced_subgraph(g,neighbors(g,i))
#     deg = degree(gneighbors)
#     V(g)$external[i] = ifelse(1 %in% deg | 0 %in% deg, 1, 0)
#   }
#   #print(V(g)$external)
#   # Before you add the edges you need to add perimeter vertex
#   g = g + vertex(nodes+1,population=0,votes_blue=0,
#                  votes_red=0,district=0,size=0,external=0)
#   # Add edge information
#   c=cbind(rep(nodes+1, sum(V(g)$external)),which(V(g)$external==1))
#   c=t(c)
#   c=as.vector(c)
#   g = g + edges(c, weight=1)
#   #print(get.edgelist(g))
#   g
# }


