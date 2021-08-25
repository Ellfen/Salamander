library(igraph)
# Using the graph_from_literal command
g1 = graph_from_literal(A-B)
g2 = graph_from_literal( A-----B )
g3 = graph_from_literal( a--b, b--c, c--d, d--e, e--a )
par(mfrow = c(1,1))
plot(g1)
plot(g2)
graph_attr(gplot,"margin") = rep(0.01,4)
par(mar=c(2,0,2,0)+0.1,mfrow=c(1,1),cex=1.5)
plot(g3, vertex.color="turquoise", main="A simple Markov Chain")
dev.copy2pdf(file="../../Images/simpleMC.pdf")
par(mfrow = c(1,1))
plot(g3)
# using make_graph
g4 = make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = FALSE)
plot(g4)
g5 = make_graph(c("A", "B", "B", "C", "C", "D"), directed = FALSE)
plot(g5)
# How would I make a 2x2 grid like this?
grid2 = make_graph(c(1,2,1,3,3,4,2,4), directed = F)
plot(grid2)
grid3 = make_graph(c(1,2,1,4,2,3,2,5,3,6,4,5,4,7,5,6,5,8,6,9,7,8,8,9), 
                   directed=F)
plot(grid3)
# back to graph_from_literal
g6 = graph_from_literal( A:B:C:D -- A:B:C:D )
plot(g6)
g7 = graph_from_literal( A:B:C:D )
plot(g7)
# Could g8 make it easier to construct the grid?
g8 = graph_from_literal(A:B:C:D - A)
plot(g8)
g9 = graph_from_literal(2:4:5:6:8 - 5); plot(g9)
g10 = make_graph( ~ A-B-C-D-A, E-A:B:C:D,
                   F-G-H-I-F, J-F:G:H:I,
                   K-L-M-N-K, O-K:L:M:N,
                   P-Q-R-S-P, T-P:Q:R:S,
                   B-F, E-J, C-I, L-T, O-T, M-S,
                   C-P, C-L, I-L, I-P)
plot(g10)
# Not sure what this shape is

# Will it be easier to make a grid using an adjacency matrix
adjm = matrix(c(0,1,1,1,0,1,1,1,0), nrow=3, ncol=3, byrow=T); adjm
g11 <- graph_from_adjacency_matrix( adjm, mode="undirected" )
plot(g11)

# To do a 3x3 grid like this
adjm = matrix(c(0,1,0,1,0,0,0,0,0,
                1,0,1,0,1,0,0,0,0,
                0,1,0,0,0,1,0,0,0,
                1,0,0,0,1,0,1,0,0,
                0,1,0,1,0,1,0,1,0,
                0,0,1,0,1,0,0,0,1,
                0,0,0,1,0,0,0,1,0,
                0,0,0,0,1,0,1,0,1,
                0,0,0,0,0,1,0,1,0), nrow=9, ncol=9, byrow=T); adjm
g12 = graph_from_adjacency_matrix(adjm, mode="lower")
plot(g12)

# Are all grid matrices the same with banded diagonal form?
# Lets try a 2x2 grid which requires a 4x4 adjacency matrix
grid2 <- matrix(0,4,4); grid2
n = 2^2
diag(grid2[-1,])<-c(1,0,1); grid2
#diag(gridm[,-1])<-c(1,0,1); gridm
diag(grid2[-c(1:2),])<-c(1,1); grid2
#diag(gridm[,-c(1:2)])<-c(1,1); gridm
g = graph_from_adjacency_matrix(grid2, mode="lower")
plot(g)
# Note how it is only necessary to have a representation of half the matrix

# Lets try a 4x4 grid
n = 4^2
grid4 <- matrix(0,n,n); grid4
diag(grid4[-1,])<-c(1,1,1,0,1,1,1,0,1,1,1,0,1,1,1); grid4
diag(grid4[-c(1:sqrt(n)),])<-rep(1,length(diag(grid4[-c(1:sqrt(n)),]))); grid4
g = graph_from_adjacency_matrix(grid4, mode="lower")
plot(g)

# Now lets make it generic
# I will use n as the final grid size so above n = 4 NOT 4^2
# I will use nodes = n^2 to represent the number of nodes
n = 4
nodes = n^2
adjm = matrix(0, nodes, nodes); adjm
diag(adjm[-1,]) = c(rep(c(rep(1,n-1),0),n-1),rep(1,n-1)); adjm
diag(adjm[-c(1:n),]) = rep(1,nodes-n); adjm
g = graph_from_adjacency_matrix(adjm, mode="lower")
plot(g)
g[1]

edges = c(2,1)
for (i in 3:nodes) {
  col2=which(adjm[i,]==1); col2
  col1=rep(i,length(col2)); col1
  X = cbind(col1, col2); X
  edges = rbind(edges, X)
}
edges

v = 1:16
dim(v) = c(4,4); v
diag(v[-c(1,2),])
diag(v[,-c(1,2)])

# To create grid from a data frame - example taken from the igraph manual
## A simple example with a couple of actors
## The typical case is that these tables are read in from files....
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)

## The opposite operation
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")
plot(g)
# In this example I have 2 dataframes - one contains a list of vertices/nodes
# with a number of attributes and the other contains edge data with edge data.
# If I was to read in a true shapefile I could theoretically have a dataframe 
# with a list of ward data and a second data frame with the adjacency information.
# HOW WOULD I GET THE ADJACENCY INFORMATION???
# I have spent time constructing a nxn grid with an adjacency matrix
actors$age[actors$name=="Alice"]

# Can I use edges matrix to plot the grid
g = graph_from_data_frame(edges, directed = FALSE, vertices = 1:nodes)
plot(g)
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")
