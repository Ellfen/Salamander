# The aim of this script is to experiment with partitioning a graph by using
# a district index to keep track of which district a precinct is in. I will
# use a dataframe to store the names, population size, voting pattern, and
# partitioning of imaginary precincts. I will generate a separate dataframe of 
# edge information where each edge has a length of 1.
######################## IMPORT FUNCTIONS AND LIBRARIES ########################
library(igraph)
library(tictoc)
source("grid_functions.R")
source("district_rules_functions.R")
# Acceptance probability function f.alpha
f.alpha = function(Qxy,Qyx,score_x,score_y,admissible){
  min((Qyx*score_y*admissible)/(Qyx*score_x), 1)
}
f.Q = function(conflicts){1/(2*length(conflicts))}
f.Jp = function(G,district,Ndist,pop_ideal){
  pop_dist = numeric(Ndist)
  for (i in 1:Ndist) {
    pop_dist[i] = sum(vertex_attr(G,"population")[which(district==i)])
  }
  #cat("Ndist =",Ndist,"pop_ideal =",pop_ideal,"pop_dist = ",pop_dist,sep=" ")
  Jp = sum((pop_dist/pop_ideal-1)^2)
  
}
############################ SETUP THE NETWORK ################################
# select grid = 0,1,2 for real map, sq, hex respectively.
grid = 2
n = 50 # grid size
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
  realmap = read.csv("Data/realwarddata.csv",header=T)
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

################################# CONSTANTS ###################################
# population constants
pop_ideal = sum(vertex_attr(g,"population"))/Ndist
deviation = 0.3
ubound = pop_ideal+(deviation*pop_ideal/2)
lbound = pop_ideal-(deviation*pop_ideal/2)
pop_bound = c(lbound,ubound); pop_bound
############################## BOUNDARY FLIP ##################################
plot(g, vertex.color=V(g)$district, vertex.frame.color=V(g)$district,
     edge.color=get.edge.attribute(g,"p1"))
beta=1
# step 1 - vector of conflicting edges
conflict_x = which(get.edge.attribute(g,"p1") != get.edge.attribute(g,"p2"))
# sample uniformly from all conflicting edges
c.edge = sample(conflict_x,1)
# choose with p=0.5 which vertex to flip (only a choice of two)
flip = sample(1:2,1)
# variable to store ID of vertex to be flipped
v.flip = Elist[c.edge,flip]
# variables to store ID of current district and proposed district of v.flip
dist_x = ifelse(flip==1,E(g)$p1[c.edge],E(g)$p2[c.edge])
dist_y = ifelse(flip==1,E(g)$p2[c.edge],E(g)$p1[c.edge])
# Create vectors to temporarily store the division information while you
# check if the new districting obeys rules
temp_dist = V(g)$district
Jpx = f.Jp(g,temp_dist,Ndist,pop_ideal)
score_x = exp(-beta*Jpx)
temp_dist[v.flip] = dist_y
temp_p1 = E(g)$p1
temp_p2 = E(g)$p2
temp_p1[which(Elist[,1]==v.flip)] = temp_dist[v.flip]
temp_p2[which(Elist[,2]==v.flip)] = temp_dist[v.flip]
# Check if proposal is contigous
tic()
contigous1 = f.is.contigous1(dist_x,g,temp_dist)
toc()
tic()
contigous2 = f.is.contigous2(dist_x,g,temp_dist)
toc()
balanced = f.is.balanced(c(dist_x,dist_y),g,temp_dist,pop_bound,pop_ideal)
admissible = as.integer(contigous1 & balanced)
Jpy = f.Jp(g,temp_dist,Ndist,pop_ideal)
score_y = exp(-beta*Jpy)
# Calculate the acceptance function - using uniform score for now
Qxy = f.Q(conflict_x)
conflict_y = which(temp_p1 != temp_p2)
Qyx = f.Q(conflict_y)
alpha = f.alpha(Qxy,Qyx,score_x,score_y,admissible)
# generate a uniform random variable
U = runif(1)
cat("edge =",Elist[c.edge,],"v.flip =",v.flip,"contig1 =",contigous1,
    "contig2 =",contigous2, "balanced =",balanced,"admissible =",admissible, 
    "U =",round(U,3),"alpha =",round(alpha,3),sep=" ","\n")
# Accept or reject proposal
if (U <= alpha){
  V(g)$district[v.flip] = dist_y
  E(g)$p1 = temp_p1
  E(g)$p2 = temp_p2
} else {
  V(g)$district[v.flip] = dist_x
  E(g)$p1 = E(g)$p1
  E(g)$p2 = E(g)$p2
}


plot(g, vertex.color=V(g)$district, vertex.frame.color=V(g)$district)

# Boundary flip in a loop
N = 1000
tic()
for (i in 1:N) {
  # step 1 - vector of conflicting edges
  conflict_x = which(get.edge.attribute(g,"p1") != get.edge.attribute(g,"p2"))
  # sample uniformly from all conflicting edges
  c.edge = sample(conflict_x,1)
  # choose with p=0.5 which vertex to flip (only a choice of two)
  flip = sample(1:2,1)
  # variable to store ID of vertex to be flipped
  v.flip = Elist[c.edge,flip]
  # variables to store ID of current district and proposed district of v.flip
  dist_x = ifelse(flip==1,E(g)$p1[c.edge],E(g)$p2[c.edge])
  dist_y = ifelse(flip==1,E(g)$p2[c.edge],E(g)$p1[c.edge])
  # Create vectors to temporarily store the division information while you
  # check if the new districting obeys rules
  temp_dist = V(g)$district
  Jpx = f.Jp(g,temp_dist,Ndist,pop_ideal)
  score_x = exp(-beta*Jpx)
  temp_dist[v.flip] = dist_y
  temp_p1 = E(g)$p1
  temp_p2 = E(g)$p2
  temp_p1[which(Elist[,1]==v.flip)] = temp_dist[v.flip]
  temp_p2[which(Elist[,2]==v.flip)] = temp_dist[v.flip]
  # Check if proposal is contigous
  contigous1 = f.is.contigous1(dist_x,g,temp_dist)
  balanced = f.is.balanced(c(dist_x,dist_y),g,temp_dist,pop_bound,pop_ideal)
  admissible = as.integer(contigous1 & balanced)
  Jpy = f.Jp(g,temp_dist,Ndist,pop_ideal)
  score_y = exp(-beta*Jpy)
  # Calculate the acceptance function - using uniform score for now
  Qxy = f.Q(conflict_x)
  conflict_y = which(temp_p1 != temp_p2)
  Qyx = f.Q(conflict_y)
  alpha = f.alpha(Qxy,Qyx,score_x,score_y,admissible)
  # generate a uniform random variable
  U = runif(1)
  # Accept or reject proposal
  if (U <= alpha){
    V(g)$district[v.flip] = dist_y
    E(g)$p1 = temp_p1
    E(g)$p2 = temp_p2
  } else {
    V(g)$district[v.flip] = dist_x
    E(g)$p1 = E(g)$p1
    E(g)$p2 = E(g)$p2
  }
}
toc()
# takes 29.63 to do 100 steps with conflict_x inside the loop for 25x25 grid
# takes the same with conflict_x outside the loop 

png("grid.png")
plot(g, vertex.color=V(g)$district, vertex.frame.color=V(g)$district)
dev.off(); graphics.off()



