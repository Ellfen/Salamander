# Script to make a grid of n connected hexagons
library(igraph)
source("../grid_functions.R")

# A single hexagon
hex1 = make_graph(c(1,2,1,3,1,4,1,5,1,6,1,7,7,2,2,3,3,4,4,5,5,6,6,7),
                  directed = F)
par(mar=c(2,0,2,0)+0.1,mfrow=c(1,1))
plot(hex1, vertex.color="Turquoise",vertex.size=20)
get.edgelist(hex1)
adjm = as.matrix(as_adjacency_matrix(hex1))
adjm

# A double hexagon
hex2 =  make_graph(c(1,2,1,3,1,4,1,5,1,6,1,7,7,2,2,3,3,4,4,5,5,6,6,7,
                     8,2,8,3,8,9,8,10,8,11,8,12,3,9,9,10,10,11,11,12,12,2),
                   directed = F)
plot(hex2)
as_adjacency_matrix(hex2)

hex2b = make_graph(c(1,2,1,3,1,4,1,5,1,6,1,7,7,2,2,3,3,4,4,5,5,6,6,7,
                     8,4,8,5,8,9,8,10,8,11,8,12,5,9,9,10,10,11,11,12,12,4),
                   directed = F)
plot(hex2b)
adjm2b = as.matrix(as_adjacency_matrix(hex2b))
adjm2b
adjm2b[1:7,8:12]
t(adjm2b[1:7,8:12])

l = make_lattice(c(c(2,2),c(2,1)))
plot(l)
# just makes square grid. Trying with vector inputs makes it 3D but can look like
# a hex when flat

# Generic matrix as seen on 
# https://newbedev.com/how-to-build-a-neighbor-table-for-the-hexagonal-lattice

n = 7
nodes = n^2
adjm = matrix(0, nodes, nodes)
adjm
diag(adjm[-1,]) = c(rep(c(rep(1,n-1),0),n-1),rep(1,n-1))
adjm
diag(adjm[-c(1:(n+1)),]) = c(rep(c(rep(1,n-1),0),n-2),rep(1,n-1))
diag(adjm[-c(1:n),]) = rep(1,nodes-n)
adjm
hexes = graph_from_adjacency_matrix(adjm, mode="lower")
plot(hexes,layout=layout_on_grid(hexes,width=sqrt(nodes),height = sqrt(nodes)),
     vertex.size=10)
neighbors(hexes,1)
neighbors(hexes,5)

# Testing the function
n=4
hexes = graph_from_adjacency_matrix(f.adjm.hex(n), mode="lower")
plot(hexes,layout=layout_on_grid(hexes,width=n,height = n),
     vertex.size=10)
neighbors(hexes,1)
neighbors(hexes,3)
f.adjm.hex(n)

# Can you build it using blocks
block1 = matrix(0, 6, 6)
block1
diag(block1[-1,]) = rep(1,5)
diag(block1[,-1]) = rep(1,5)
block1
block2 = diag(6)
block2
diag(block2[,-1]) = rep(1,5)
block2
block2
block3=t(block2)
block3
block4=matrix(0,6,6)
M1 = rbind(block1,block2,block4,block4)
M2 = rbind(t(block2),block1,block2,block4)
M3 = rbind(block4,t(block2),block1,block2)
M4 = rbind(block4,block4,t(block2),block1)
M = cbind(M1,M2,M3,M4)
hexes = graph_from_adjacency_matrix(M, mode="upper")
plot(hexes,layout=layout_on_grid(hexes,width=6,height = 6))
M

