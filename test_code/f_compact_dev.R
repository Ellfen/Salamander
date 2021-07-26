# This script implements code to measure the compactness of a district.
# In order to measure compactness need a measure of the boundary length.
# Two functions will be implemented to test compactness - one that works with
# a grid and one that works with the graph created from a real shape file.
# These inputs can be swapped between by setting binary indicator grid to 1
# if using a grid of 0 is using dummy shapefile adjacency matrix.
library(igraph)
source("../network_functions.R")

############################ SETUP THE NETWORK ################################
# Wrap this up in a function as you use it at the start of every program.
# The input will be 0,1,2 for graph_type.
# select grid = 0,1,2 for real map, sq, hex respectively.
grid = 2
n = 4
Ndist = 4
Ncounty = 4

gplot = f.graph(n,grid,Ndist,Ncounty,"../")
nodes = vcount(gplot)
g = f.perimeter(gplot,nodes)
V(g)$district = c(1,1,2,2,1,1,2,2,3,3,4,4,3,3,4,4,0)

# Setup the plotting information
V(gplot)$size = 10
if (n > 20) {
  V(gplot)$size = 1 # Reduce size of the nodes
  V(gplot)$label = NA # Do not print labels
}

if (grid == 0) {
  graph_attr(gplot,"layout") = layout_with_graphopt(gplot, charge=0.0001, 
                                                    mass=30,
                                                    spring.length = 0,
                                                    spring.constant = 1)
  # layout_with_fr is another option worth exploring
  # layout does not always give nice results - might to to do several iterations
  # to get a good one. That completely avoids overlaps.
} else {
  graph_attr(gplot,"layout") = layout_on_grid(gplot, width = n, height = n, 
                                              dim = 2)
}

graph_attr(gplot,"margin") = rep(0.01,4)
par(mar=c(0.5,0.5,0.5,0.5)+0.1)
plot(gplot, vertex.color=get.vertex.attribute(g,"district"), 
     vertex.frame.color=get.vertex.attribute(g,"district"),
     edge.color=get.edge.attribute(g,"p1"))

# The edgelist won't change so create it now
Elist = get.edgelist(g)
class(Elist) = "numeric"
E(g)$p1 = V(g)$district[Elist[,1]]
E(g)$p2 = V(g)$district[Elist[,2]]
# Make a note of which edges are perimeter edges
Eint = min(which(Elist[,2]==nodes+1))

# The graph has been updated with a note of vertices that form the boundary. To
# get district boundaries I want to make a note of all conflicting edges and
# all boundary edges.
# The below code returns the a vector of boundary lengths for each district
# Also need district areas
f.compact = function(G,district) {
  Ndist = length(unique(district))-1
  boundary = area = numeric(Ndist)
  for (i in 1:Ndist) {
    gsub = subgraph.edges(G,c(which(E(G)$p1==i),which(E(G)$p2==i)))
    conflicts = E(gsub)[which(E(gsub)$p1 != E(gsub)$p2)]
    boundary[i] = sum(E(gsub)$weight[conflicts])
    area[i] = sum(V(G)$area[which(V(G)$district==i)])
  }
  Ji = sum(boundary^2/area)
  Ji
}
Ji = f.compact(g,V(g)$district)

########################## CODE FOR PERIMETER NODES ###########################
# The first step is to determine the perimeter of the full map. For each node
# need to determine if if is an external node. For a hex grid and real map use
# the girth of the subgraph created from its neighbours.
for (i in 1:length(V(g))) {
  gneighbors = induced_subgraph(g,neighbors(g,i))
  deg = degree(gneighbors)
  V(g)$external[i] = ifelse(1 %in% deg | 0 %in% deg, 1, 0)
}
V(g)$external
vertex_attr_names(g)
# Before you add the edges you need to add vertex 0
g = g + vertex(nodes+1,population=0,votes_blue=0,
               votes_red=0,district=0,size=0,external=0)

V(g)$name
V(g)$population
V(g)$size
# Note that V(g) is indexed from 1 and the new edge has been added to the end of
# end of the list.

c=cbind(rep(nodes+1, sum(V(g)$external)),which(V(g)$external==1)); c
# How can I flatten this?

get.edgelist(g)
is.matrix(c)
c=t(c)
c=as.vector(c)
c
g = g + edges(c)
get.edgelist(g)

# The edgelist won't change so create it now
# graph_elist = get.edgelist(g)
# df_elist = E.data[,1:2]
# df_elist = df_elist[c("v2","v1")]
# df_elist
# df_elist = as.matrix(df_elist)
# is.matrix(graph_elist)
# class(graph_elist)="numeric"
# graph_elist == df_elist

########################### OLDER DEVELOPMENT CODE #############################

# You want to add information for all nodes that form the perimeter of the full 
# map.
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

# But can I use girth for a real map graph? (THIS CODE ONLY WORKS WHEN GRID=0,2)
vid = neighbors(g,12); vid
gcycle = induced_subgraph(g,vid)
girth(gcycle)
radius(gcycle)
diameter(gcycle)
eccentricity(gcycle)
# This has girth equal to the number of neighbors.
vid = neighbors(g,1); vid
gcycle = induced_subgraph(g,vid)
girth(gcycle)
radius(gcycle)
diameter(gcycle)
eccentricity(gcycle)
# This has a girth of zero as there are no cyclical paths.
vid = neighbors(g,15); vid
gcycle = induced_subgraph(g,vid)
girth(gcycle)
radius(gcycle)
diameter(gcycle)
eccentricity(gcycle)
degree(gcycle)
# The girth is not equal to the number of neighbors
vid = neighbors(g,9); vid
gcycle = induced_subgraph(g,vid)
girth(gcycle)
eccentricity(gcycle)
radius(gcycle)
diameter(gcycle)
degree(gcycle)
vid = neighbors(g,23); vid
gcycle = induced_subgraph(g,vid)
girth(gcycle)
eccentricity(gcycle)
radius(gcycle)
diameter(gcycle)
degree(gcycle)


# Old code for how to get what you thought was the boundary of a district but 
# you were wrong.
#for (i in 1:Ndist) {
# i = 2
# k = 3
#   D = as.vector(V(g)[which(V(g)$district==i | V(g)$district==k)])
#   gsub = induced_subgraph(g,D)
#   for (j in 1:length(D)) {
#     gneighbors = induced_subgraph(gsub,neighbors(gsub,j))
#     V(gsub)$boundary[j] = ifelse(girth(gneighbors,circle=F)[[1]] == 0, 1, 0)
#   }

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