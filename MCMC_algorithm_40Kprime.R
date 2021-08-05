# The aim of this script is to run 40,000 steps with beta=0
# It does not matter the outcome of any of the score functions as they will 
# equal 1 however useful to still calculate and store to get idea of what
# outcome is for indiscriminant flip walk.
######################## IMPORT FUNCTIONS AND LIBRARIES ########################
library(igraph)
library(tictoc)
source("network_functions.R")
source("rule_score_functions.R")
source("distribution_functions.R")
# Acceptance probability function f.alpha
f.alpha = function(Qxy,Qyx,score_x,score_y,admissible){
  min((Qyx*score_y*admissible)/(Qxy*score_x), 1)
}
f.Q = function(conflicts){1/(2*length(conflicts))}
############################ SETUP THE NETWORK ################################
n = sqrt(2692)
Ndist = 13
Ncounty = 100
# select grid = 0,1,2,3,4 for real map, sq, hex, NCdummydata, NCrealdata.
grid = 4
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
} else if (grid == 4) {
  graph_attr(gplot,"layout") = layout=cbind(V(gplot)$centroidx,V(gplot)$centroidy)
} else {
  graph_attr(gplot,"layout") = layout_on_grid(gplot, width = n, height = n, 
                                              dim = 2)
}
vcolor = c("red","violetred1","purple3","slateblue","navy","royalblue",
           "skyblue2","turquoise3","seagreen","olivedrab2","gold",
           "darkorange","chocolate3")

graph_attr(gplot,"margin") = rep(0.01,4)
par(mar=c(0.5,0.5,0.5,0.5)+0.1)
plot(gplot, vertex.color=vcolor[get.vertex.attribute(g,"district")], 
     vertex.frame.color=vcolor[get.vertex.attribute(g,"district")],
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
deviation = 0.01
ubound = pop_ideal+(deviation*pop_ideal/2)
lbound = pop_ideal-(deviation*pop_ideal/2)
pop_bound = c(lbound,ubound); pop_bound
# county split constants
Mc = 10
# score function weightings
wp = 0
wc = 0
wi = 0
# temperature
beta = 0
#################### DOES INITIAL DISTRICT OBEY POP BALANCE ####################
balance_check = numeric(Ndist)
for (i in 1:Ndist) {
  pop = sum(vertex_attr(g,"population")[which(V(g)$district==i)])
  print(pop)
  balance_check[i] = ifelse(pop_bound[2] >= pop & pop >= pop_bound[1], 1, 0)
}
balance_check
##################### IS INITIAL DISTRICTING CONTIGUOUS ########################
contigous_check = numeric(Ndist)
for (i in 1:Ndist) {
  contigous_check[i] = f.is.contigous1(i,g,V(g)$district)
}
contigous_check
# Add edges to connect back to district?
############################## BOUNDARY FLIP ##################################
plot(gplot, vertex.color=vcolor[V(g)$district], 
     vertex.frame.color=vcolor[V(g)$district],
     edge.color=vcolor[get.edge.attribute(g,"p1")])
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
# Check if proposal is balanced - store this information
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
balanced = accepted = admissible = Jpx = Jpy = numeric(N)

tic()
for (i in 1:N) {
  print(i)
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
  # Jpx[i] = f.popscore(g,temp_dist,pop_ideal)
  # Jcx = f.countyscore(g,temp_dist,Mc)
  # Jix = f.compactscore(g,temp_dist,temp_p1,temp_p2)
  # score_x = exp(-beta*(wp*Jpx[i]+wc*Jcx+wi*Jix))
  temp_dist[v.flip] = dist_y
  temp_p1[which(Elist[,1]==v.flip)] = temp_dist[v.flip]
  temp_p2[which(Elist[,2]==v.flip)] = temp_dist[v.flip]
  # Check if proposal is contigous
  contigous1 = f.is.contigous1(dist_x,g,temp_dist)
  # Check if proposal is balanced
  # balanced[i] = f.is.balanced(c(dist_x,dist_y),g,temp_dist,pop_bound,pop_ideal)
  # admissible[i] = as.integer(contigous1)# & balanced)
  # Jpy[i] = f.popscore(g,temp_dist,pop_ideal)
  # Jcy = f.countyscore(g,temp_dist,Mc)
  # Jiy = f.compactscore(g,temp_dist,temp_p1,temp_p2)
  # score_y = exp(-beta*(wp*Jpy[i]+wc*Jcy+wi*Jiy))
  # Calculate the acceptance function - using uniform score for now
  Qxy = f.Q(conflict_x)
  conflict_y = which(temp_p1[1:(Eint-1)] != temp_p2[1:(Eint-1)])
  Qyx = f.Q(conflict_y)
  alpha = f.alpha(Qxy,Qyx,1,1,contigous1)
  # generate a uniform random variable
  U = runif(1)
  accepted[i] = ifelse(U <= alpha, 1, 0)
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
}
toc()
balanced
mean(balanced)
mean(balanced[1:500])
mean(balanced[9500:10000])
admissible
mean(admissible)
accepted
mean(accepted)
min(Jpx)
max(Jpx)
min(Jpy)
max(Jpy)
round(Jpx,4)
# Aim is for the population score to get closer and closer to 0.
exp(0)

plot(gplot, vertex.color=vcolor[V(g)$district], 
     vertex.frame.color=vcolor[V(g)$district],
     edge.color=vcolor[get.edge.attribute(g,"p1")])

png("grid.png")
plot(gplot, vertex.color=V(g)$district, vertex.frame.color=V(g)$district)
dev.off(); graphics.off()



