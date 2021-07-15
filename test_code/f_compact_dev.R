# This script implements code to measure the compactness of a district.
# In order to measure compactness need a measure of the boundary length.
# Two functions will be implemented to test compactness - one that works with
# a grid and one that works with the graph created from a real shape file.
# These inputs can be swapped between by setting binary indicator grid to 1
# if using a grid of 0 is using dummy shapefile adjacency matrix.
library(igraph)
source("../grid_functions.R")

############################ SETUP THE NETWORK ################################
# select grid = 0,1,2 for real map, sq, hex respectively.
grid = 0
n = 4 # grid size
if (grid == 0) {
  nodes = 26
} else {
  nodes = n^2
}
votes = c(rep(1,ceiling(0.4*nodes)),rep(0,floor(0.6*nodes)))
# Assign an initial partition.
Ndist = 3
district0 = rep(Ndist,nodes)
district0 = replace(district0, 1:(Ndist*floor(nodes/Ndist)),
                    rep(1:Ndist, each=(nodes/Ndist)))
P.data = data.frame(precinct = 1:nodes, population = rep(1, nodes), 
                    votes_blue = votes, votes_red = as.integer(!votes),
                    district = district0)

if (grid == 0) {
  realmap = read.csv("../Data/realwarddata.csv",header=T)
  realmap=as.matrix(realmap[,2:27])
  adjm = realmap
} else if (grid == 1) {
  adjm = f.adjm.sq(n)
} else {
  adjm = f.adjm.hex(n)
}
E.data = f.edges(adjm)
class(E.data)
E.data$weight = rep(1, dim(E.data)[1])


# I now have two data frames - one containing precinct data and one containing 
# edge connections and edge lengths.
# Make the graph object
g = graph_from_data_frame(E.data, directed = FALSE, 
                          vertices=P.data)
district_plotting = make_clusters(g,membership = V(g)$district)
class(district_plotting)

# Setup the plotting information
V(g)$size = 10
if (n > 20) {
  V(g)$size = 1 # Reduce size of the nodes
  V(g)$label = NA # Do not print labels
}

if (grid == 0) {
  graph_attr(g,"layout") = layout_with_graphopt(g, charge=0.0001, mass=30,
                                                spring.length = 0,
                                                spring.constant = 1)
  # layout_with_fr is another option worth exploring
  # layout does not always give nice results - might to to do several iterations
  # to get a good one. That completely avoids overlaps.
} else {
  graph_attr(g,"layout") = layout_on_grid(g, width = n, height = n, dim = 2)
}

graph_attr(g,"margin") = rep(0.01,4)
par(mar=c(0.5,0.5,0.5,0.5)+0.1)
plot(g, vertex.color=get.vertex.attribute(g,"district"), 
     vertex.frame.color=get.vertex.attribute(g,"district"))
# The edgelist won't change so create it now
Elist = get.edgelist(g)
class(Elist) = "numeric"
# Now add district attributes to the edges
for (i in 1:dim(E.data)[1]) {
  E(g)$p1[i] = V(g)$district[Elist[i,1]]
  E(g)$p2[i] = V(g)$district[Elist[i,2]]
}


# You want to add information for all nodes that form the boundary.
diameter(g)
radius(g)
eccentricity(g)
# Possible for a perimeter node and internal node to have the same eccentricity.
# Trying to use girth
girth(g, circle=T)
# could you make a subgraph of the neighbours and then check if that is connected?
vid = neighbors(g,7); vid
gcycle = induced_subgraph(g,vid)
is.connected(gcycle)
# This doesn't work for a grid it will not work for real map data either as 
# neighbors of a real graph are always connected. What you are really interested
# in is whether it is cyclical.

# But can I use girth for a real map graph? (THIS CODE ONLY WORKS WHEN GRID=0)
vid = neighbors(g,12); vid
gcycle = induced_subgraph(g,vid)
girth(gcycle)
# This has girth equal to the number of neighbors.
vid = neighbors(g,1); vid
gcycle = induced_subgraph(g,vid)
girth(gcycle)
# This has a girth of zero as there are no cyclical paths.

# The first step is to determine the boundary of each district. For each node
# in a district need to determine if if is a boundary node. For a hex grid and
# real map use girth of subgraph created from neighbours in same district.

#for (i in 1:Ndist) {
i = 2
k = 3
  D = as.vector(V(g)[which(V(g)$district==i | V(g)$district==k)])
  gsub = induced_subgraph(g,D)
  for (j in 1:length(D)) {
    gneighbors = induced_subgraph(gsub,neighbors(gsub,j))
    V(gsub)$boundary[j] = ifelse(girth(gneighbors,circle=F)[[1]] == 0, 1, 0)
  }

#can you modify breadth first search?
  # Choose a starting vertex uniformly from the district vertices
  v0 = sample(which(V(gsub)$boundary==1),1)
  V(gsub)$name[v0]
  # generate a vector of neighbors
  v.neighbors = as.vector(neighbors(gsub,v0))
  V(gsub)$name[v.neighbors]
  # restrict neighbors to vertices on the boundary
  # add these to a queue of vertices to search
  queue = c(v0,v.neighbors[which(V(gsub)$boundary[v.neighbors]==1)])
  V(gsub)$name[queue]
  # add queued vertices to explored vector to avoid checking them twice
  explored=queue
  
############################### UP TO HERE ##################################
  # loop until all vertices in district are searched
  while (length(queue) > 0) {
    for (i in 1:length(queue)) {
      # get neighbors
      v.neighbors = as.vector(neighbors(G,queue[1]))
      # restrict neighbors to those in district
      q = v.neighbors[which(district[v.neighbors]==distID)]
      # restrict neighbors to those not explored
      q = setdiff(q,explored)
      # update the queue
      queue = c(queue[-1],q)
      # update explored
      explored=c(explored,q)
    }
  }