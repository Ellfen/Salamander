# The aim of this script to find wp (the population score weighting)
######################## IMPORT FUNCTIONS AND LIBRARIES ########################
library(igraph)
library(tictoc)
source("network_functions.R")
source("rule_score_functions.R")
source("distribution_functions.R")
# Acceptance probability function f.alpha
f.alpha = function(Qxy,Qyx,score_x,score_y,admissible){
  min((Qyx*score_y*admissible)/(Qyx*score_x), 1)
}
f.Q = function(conflicts){1/(2*length(conflicts))}
############################ SETUP THE NETWORK ################################
n = 51
Ndist = 13
Ncounty = 100
# select grid = 0,1,2,3 for real map, sq, hex, NCdummydata respectively.
grid = 3
gplot = f.graph(n,grid,Ndist,Ncounty)
nodes = vcount(gplot)
g = f.perimeter(gplot,nodes)

# Setup the plotting information
V(gplot)$size = 10
V(gplot)$label = V(gplot)$county
if (n > 20) {
  V(gplot)$size = 2 # Reduce size of the nodes
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
################################ CONSTANTS #####################################
# population constants
pop_ideal = sum(vertex_attr(g,"population"))/Ndist
deviation = 0.03
ubound = pop_ideal+(deviation*pop_ideal/2)
lbound = pop_ideal-(deviation*pop_ideal/2)
pop_bound = c(lbound,ubound); pop_bound
# county split constants
Mc = 10
# score function weightings
wp = 1000
wc = 0
wi = 0
###################### DOES INITIAL DISTRICT OBEY POP BALANCE #################
balance_check = numeric(Ndist)
for (i in 1:Ndist) {
  pop = sum(vertex_attr(g,"population")[which(V(g)$district==i)])
  print(pop)
  balance_check[i] = ifelse(pop_bound[2] >= pop & pop >= pop_bound[1], 1, 0)
}
balance_check
############################## BOUNDARY FLIP ##################################
plot(gplot, vertex.color=V(g)$district, vertex.frame.color=V(g)$district,
     edge.color=get.edge.attribute(g,"p1"))
beta=1
# step 1 - vector of conflicting edges
conflict_x = which(E(g)$p1[1:(Eint-1)] != E(g)$p2[1:Eint-1])
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
temp_p1 = E(g)$p1
temp_p2 = E(g)$p2
Jpx = f.popscore(g,temp_dist,pop_ideal)
Jcx = f.countyscore(g,temp_dist,Mc)
Jix = f.compactscore(g,temp_dist,temp_p1,temp_p2)
score_x = exp(-beta*((wp*Jpx)+(wc*Jcx)+(wi*Jix)))
temp_dist[v.flip] = dist_y
temp_p1[which(Elist[,1]==v.flip)] = temp_dist[v.flip]
temp_p2[which(Elist[,2]==v.flip)] = temp_dist[v.flip]
# Check if proposal is contigous
contigous1 = f.is.contigous1(dist_x,g,temp_dist)
# Check if proposal is balanced
balanced = f.is.balanced(c(dist_x,dist_y),g,temp_dist,pop_bound,pop_ideal)
# Removing balanced from admissible function
admissible = as.integer(contigous1)# & balanced)
Jpy = f.popscore(g,temp_dist,pop_ideal)
Jcy = f.countyscore(g,temp_dist,Mc)
Jiy = f.compactscore(g,temp_dist,temp_p1,temp_p2)
score_y = exp(-beta*(wp*Jpy+wc*Jcy+wi*Jiy))
# Calculate the acceptance function - using uniform score for now
Qxy = f.Q(conflict_x)
conflict_y = which(temp_p1[1:(Eint-1)] != temp_p2[1:(Eint-1)])
Qyx = f.Q(conflict_y)
alpha = f.alpha(Qxy,Qyx,score_x,score_y,admissible)
# generate a uniform random variable
U = runif(1)
cat("edge =",Elist[c.edge,],"v.flip =",v.flip,"contig =",contigous1, 
    "balanced =",balanced,"admissible =",admissible, 
    "U =",round(U,3),"alpha =",round(alpha,3),sep=" ","\n")
cat("Jpx=",round(Jpx,5),"Jcx=",round(Jcx,3),"Jix=",round(Jix,3),
    "score_x=",score_x,"\n",
    "Jpy=",round(Jpy,5),"Jcy=",round(Jcy,3),"Jiy=",round(Jiy,3),
    "score_y=",score_y,sep=" ","\n")
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

# Boundary flip in a loop
N = 10
# is balanced check
balanced = numeric(N)
#green_seats = numeric(N)
tic()
for (i in 1:N) {
  ################################ FLIP WALK ##################################
  # step 1 - vector of conflicting edges
  conflict_x = which(E(g)$p1[1:(Eint-1)] != E(g)$p2[1:Eint-1])
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
  temp_p1 = E(g)$p1
  temp_p2 = E(g)$p2
  Jpx = f.popscore(g,temp_dist,pop_ideal)
  Jcx = f.countyscore(g,temp_dist,Mc)
  Jix = f.compactscore(g,temp_dist,temp_p1,temp_p2)
  score_x = exp(-beta*(wp*Jpx+wc*Jcx+wi*Jix))
  temp_dist[v.flip] = dist_y
  temp_p1[which(Elist[,1]==v.flip)] = temp_dist[v.flip]
  temp_p2[which(Elist[,2]==v.flip)] = temp_dist[v.flip]
  # Check if proposal is contigous
  contigous1 = f.is.contigous1(dist_x,g,temp_dist)
  # Check if proposal is balanced
  balanced[i] = f.is.balanced(c(dist_x,dist_y),g,temp_dist,pop_bound,pop_ideal)
  admissible = as.integer(contigous1)# & balanced)
  Jpy = f.popscore(g,temp_dist,pop_ideal)
  Jcy = f.countyscore(g,temp_dist,Mc)
  Jiy = f.compactscore(g,temp_dist,temp_p1,temp_p2)
  score_y = exp(-beta*(wp*Jpy+wc*Jcy+wi*Jiy))
  # Calculate the acceptance function - using uniform score for now
  Qxy = f.Q(conflict_x)
  conflict_y = which(temp_p1[1:(Eint-1)] != temp_p2[1:(Eint-1)])
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
  ##########################################################################
  
  ############################ DISTRIBUTIONS ###############################
  red_seats[i] = f.seat.count(g,Nparty,party,"red")
  blue_seats[i] = f.seat.count(g,Nparty,party,"blue")
  #green_seats[i] = f.seat.count(g,Nparty,party,"green")
}
toc()
#red_seats
par(mfrow=c(1,1),mar=c(3,3,3,3)+0.1)
hist(red_seats, breaks=seq(min(red_seats)-0.5, max(red_seats)+0.5, by=1)  )
#red_seats
hist(blue_seats, breaks=seq(min(blue_seats)-0.5, max(blue_seats)+0.5, by=1)  )
#blue_seats
#hist(green_seats, breaks=seq(min(green_seats)-0.5, max(green_seats)+0.5, by=1)  )
#green_seats
plot(gplot, vertex.color=V(g)$district, vertex.frame.color=V(g)$district,
     edge.color=get.edge.attribute(g,"p1"))

png("grid.png")
plot(gplot, vertex.color=V(g)$district, vertex.frame.color=V(g)$district)
dev.off(); graphics.off()



