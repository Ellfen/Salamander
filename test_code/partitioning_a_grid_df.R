# The aim of this script is to experiment with partitioning a graph by using
# a district_index to keep track of which district a precinct is in. I will
# use a dataframe to store the names, population size, and voting pattern of 
# imaginary precinct. I will generate a separate dataframe of edge information 
# where each edge has a length of 1.
library(igraph)
source("grid_functions.R")
source("district_rules_functions.R")
n = 4 # grid size
nodes = n^2
votes = c(rep(1,ceiling(0.4*nodes)),rep(0,floor(0.6*nodes)))
# Assign an initial partition.
partition0 = c(rep(1,ceiling(nodes/2)),rep(2,floor(nodes/2)))
P.data = data.frame(precinct = 1:nodes, population = rep(1, nodes), 
                    votes_blue = votes, votes_red = as.integer(!votes),
                    partition = partition0)
P.data
adjm = f.adjm(n)
E.data = f.edges(adjm)
E.data
class(E.data)
E.data$length = rep(1, sum(adjm))
E.data
# I now have two data frames - one containing precinct data and one containing 
# edge connections and edge lengths. I now want to try to plot it with different
# shapes for blue/red voters.
g = graph_from_data_frame(E.data, directed = FALSE, 
                          vertices=P.data)
v.shape = ifelse(P.data[,"votes_blue"]==1,"circle","square")
par(mfrow=c(1,1))
l1 = rep(1:n, each=n)
l2 = rep(1:n, times=n)
plot(g, vertex.shape=v.shape, vertex.color=get.vertex.attribute(g,"partition"), 
     layout=cbind(l1,l2))
plot(g, vertex.shape=v.shape, vertex.color=P.data$partition, 
     layout=layout_on_grid(g))
# Not sure if I like this - maybe I could use the labels instead? It is not
# relevant for now however.

# Now lets attempt to employ the boundary flip partition. To begin with I will
# use a uniform score function.
# Step 1 is to select a conflicting edge uniformly from all conflicting edges.
# What is the best way?
for (i in 1:sum(adjm)) {
  E.data$p1[i] = P.data$partition[E.data$col1[i]]
  E.data$p2[i] = P.data$partition[E.data$col2[i]]
}

# To work through the method step by step
# conflicts gives me the indices of all conflicted edges.
conflicts = which(E.data$p1 != E.data$p2)
# sample uniformly from all conflicting edges
c.edge = sample(conflicts,1)
E.data[c.edge,]
# choose with p=0.5 which vertex to flip (only a choice of two)
flip = sample(1:2,1)
flip 
# update the partition assignment of the vertex you chose to flip. Since flip
# is either 1 or 2 6-flip selects the appropriate column in E.data.
old_dist = E.data[c.edge,3+flip]; old_dist
new_dist = E.data[c.edge,6-flip]; new_dist
P.data[E.data[c.edge,flip],"partition"] = new_dist
plot(g, vertex.color=P.data$partition, layout=layout_on_grid(g))
# Check that the district which lost a vertex is still connected.
gsub = induced_subgraph(g, P.data$precinct[which(P.data$partition==old_dist)],
                        impl="copy_and_delete")
plot(gsub, vertex.color=old_dist, layout=layout_on_grid(g))
# Check that the subgraph is connected
edge_connectivity(gsub)
# Update the partition columns of E.data
E.data$p1[which(E.data$col1==E.data[c.edge,flip])] = 
  P.data$partition[E.data$col1[which(E.data$col1==E.data[c.edge,flip])]]
E.data$p2[which(E.data$col2==E.data[c.edge,flip])] = 
  P.data$partition[E.data$col2[which(E.data$col2==E.data[c.edge,flip])]]
E.data
# Can I create two subgraphs
g1 = induced_subgraph(g, P.data$precinct[which(P.data$partition==1)])
plot(g1)
edge_connectivity(g1)
gtest = make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = FALSE)
plot(gtest)
edge_connectivity(gtest)
# This suggests I can use edge connectivity to determine if a graph is connected.

# Boundary flip in a loop
N = 100
for (i in 1:N) {
  # conflicts gives me the indices of all conflicted edges.
  conflicts = which(E.data$p1 != E.data$p2)
  # sample uniformly from all conflicting edges
  c.edge = sample(conflicts,1)
  # choose with p=0.5 which vertex to flip (only a choice of two)
  flip = sample(1:2,1)
  # update the partition assignment of the vertex you chose to flip. Since flip
  # is either 1 or 2 6-flip selects the appropriate column in E.data.
  P.data[E.data[c.edge,flip],"partition"] = E.data[c.edge,6-flip]
  # Update the partition columns of E.data
  E.data$p1[which(E.data$col1==E.data[c.edge,flip])] = 
    P.data$partition[E.data$col1[which(E.data$col1==E.data[c.edge,flip])]]
  E.data$p2[which(E.data$col2==E.data[c.edge,flip])] = 
    P.data$partition[E.data$col2[which(E.data$col2==E.data[c.edge,flip])]]
}
plot(g,vertex.color=P.data$partition)

