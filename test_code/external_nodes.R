# To update f.edges to include edges for external nodes
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

for(i in 1:nodes) {
  print(i)
  print(eigen(as_adjacency_matrix(g)[as.vector(neighbors(g,i)),
                                     as.vector(neighbors(g,i))])$values)
}

for(i in 1:nodes) {
  print(i)
  print(eigen(as_adjacency_matrix(g)[i,i])$values)
}


