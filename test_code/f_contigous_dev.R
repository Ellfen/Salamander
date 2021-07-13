#How to check if a graph is connected
library(igraph)
source("grid_functions.R")
source("district_rules_functions.R")

n = 6 # grid size
nodes = n^2
P.data = data.frame(precinct = 1:nodes, 
                    partition = c(rep(1:4,each=8),1,1,1,1))
adjm = f.adjm(n)
E.data = f.edges(adjm)
g = graph_from_data_frame(E.data, directed = FALSE, vertices=P.data)
plot(g, vertex.color=P.data$partition, layout=layout_on_grid(g))
temp = V(g)$partition
for (i in 1:4){
  c = f.is.contigous1(i,g,temp)
  print(c)
}

# How can I check all the blues/yellows are connected?
# Specify the district to check
distID = 4
# Choose a starting vertex uniformly from the district vertices
v0 = sample(P.data$precinct[which(P.data$partition==distID)],1)
# generate a vector of neighbors
v.neighbors = as.vector(neighbors(g,v0))
# restrict neighbors to vertices in the same district
# add these to a queue of vertices to search
queue = v.neighbors[which(vertex_attr(g,"partition",v.neighbors)==distID)]
# add queued vertices to explored vector to avoid checking them twice
explored=queue
# loop until all vertices in district are searched
while (length(queue) > 0) {
  for (i in 1:length(queue)) {
    # get neighbors
    v.neighbors = as.vector(neighbors(g,queue[1]))
    # restrict neighbors to those in district
    q = v.neighbors[which(vertex_attr(g,"partition",v.neighbors)==distID)]
    # restrict neighbors to those not explored
    q = setdiff(q,explored)
    # update the queue
    queue = c(queue[-1],q)
    # update explored
    explored=c(explored,q)
  }
}
# each district should have a unique sum - check if the sum of vertices in the
# district matches the sum of vertices explored
check1 = as.integer(sum(explored)== 
                         sum(P.data$precinct[which(P.data$partition==distID)]))
# As a backup also check if the length of explored equals no.of vertices
check2 = as.integer(length(explored)==
                      length(P.data$precinct[which(P.data$partition==distID)]))
# If both checks are true then the district is connected
is.contigous = check1 & check2
is.contigous

# How to check if the populations of districts are with given percentage of each
# other.

#using induced_subgraph
distID = 4
district = V(g)$partition
district
block = induced_subgraph(g,which(district==distID))
list.vertex.attributes(block)
V(block)$name
is.connected(block)
