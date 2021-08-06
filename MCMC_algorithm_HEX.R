# The aim of this script to find wp (the population score weighting)
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
# select grid = 0,1,2,3,4,5 for real map, sq, hex, NCdummy, NCreal, NCsub
grid = 2
if (grid == 5) {
  n = sqrt(1060)
  Ndist = 5
  Ncounty = 38 
} else if (grid == 2) {
  n = 4
  Ndist = 3
  Ncounty = 4
} else {
  
}

gplot = f.graph(n,grid,Ndist,Ncounty)
nodes = vcount(gplot)
g = gplot
# Do not add perimeter info
# g = f.perimeter(gplot,nodes)

# Absorb discontiguous nodes
if (grid == 5) {
  V(g)$district[which(V(g)$name=="197")] = 1
  V(g)$district[which(V(g)$name=="208")] = 1
  V(g)$district[which(V(g)$name=="221")] = 1
  V(g)$district[which(V(g)$name=="218")] = 1
}

# Setup the plotting information
V(gplot)$size = 15
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
} else if (grid == 4 | grid == 5 | grid == 2) {
  graph_attr(gplot,"layout") = cbind(V(gplot)$centroidx,V(gplot)$centroidy)
  #l = cbind(V(gplot)$centroidx,V(gplot)$centroidy)
} else {
  graph_attr(gplot,"layout") = layout_on_grid(gplot, width = n, height = n, 
                                              dim = 2)
}

# vcolor = c("red","violetred1","deeppink3","purple","navy","royalblue",
#              "deepskyblue","turquoise3","seagreen","olivedrab2","gold",
#              "darkorange1","orange")

vcolor = c("turquoise3","gold","olivedrab3","royalblue","darkorange","grey","red")
#vshape = c("circle","square","rectangle","none")



graph_attr(gplot,"margin") = rep(0.01,4)
par(mar=c(2,0,2,0)+0.1,mfrow=c(1,1))
plot(gplot, vertex.color=vcolor[get.vertex.attribute(g,"district")], 
     vertex.frame.color=vcolor[get.vertex.attribute(g,"district")],
     asp=0)
axis(1)
axis(2,pos=-1)
abline(h=1)
abline(h=-1)
abline(h=0)
# The edgelist won't change so create it now
Elist = get.edgelist(g)
class(Elist) = "numeric"
E(g)$p1 = V(g)$district[Elist[,1]]
E(g)$p2 = V(g)$district[Elist[,2]]
# Make a note of which edges are perimeter edges
# Eint = min(which(Elist[,2]==nodes+1))
################################ CONSTANTS #####################################
# population constants
pop_ideal = sum(vertex_attr(g,"population"))/Ndist
deviation = 0.7
ubound = pop_ideal+(deviation*pop_ideal/2)
lbound = pop_ideal-(deviation*pop_ideal/2)
pop_bound = c(lbound,ubound); pop_bound
# county split constants
Mc = 100
# score function weightings
wp = 1
wc = 1
wi = 1
# temperature
beta = 1
###################### DOES INITIAL DISTRICT OBEY POP BALANCE #################
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

# Plot before starting 
# internal edges are color - conflicting edges are grey
edge_color = ifelse(E(g)$p1 != E(g)$p2,6,E(g)$p1)
district = V(g)$district
par(mfrow=c(2,2),mar=c(2,1.3,2,0.5)+0.1)
plot(gplot, asp=1, vertex.color=vcolor[district], 
     vertex.frame.color=vcolor[district],
     edge.color=vcolor[edge_color], main="Initial Districting")
legend("topright",legend=c("District 1","District 2", "District 3"),
       col=vcolor[1:3],pch=19,bty="n")
axis(1)
axis(2,pos=-1.1)

a = c(1525,1527,1528)
for (j in 1:3) {
set.seed(a[j])
# Boundary flip in a loop
N = 1
# information to store
balanced = accepted = admissible = numeric(N)
Jpx = Jpy = Jcx = Jcy = Jix = Jiy = numeric(N)
tic()
for (i in 1:N) {
  print(i)
  ################################ FLIP WALK ##################################
  # step 1 - vector of conflicting edges
  conflict_x = which(E(g)$p1 != E(g)$p2) #which(E(g)$p1[1:(Eint-1)] != E(g)$p2[1:Eint-1])
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
  Jpx[i] = f.popscore(g,temp_dist,pop_ideal,Ndist)
  Jcx[i] = f.countyscore(g,temp_dist,Mc,Ncounty)
  Jix[i] = f.roeck(g,temp_dist,Ndist,grid)
  score_x = exp(-beta*((wp*Jpx[i])+(wc*Jcx[i])+(wi*Jix[i])))
  temp_dist[v.flip] = dist_y
  temp_p1[which(Elist[,1]==v.flip)] = temp_dist[v.flip]
  temp_p2[which(Elist[,2]==v.flip)] = temp_dist[v.flip]
  # Check if proposal is contigous
  contigous1 = f.is.contigous1(dist_x,g,temp_dist)
  # Check if proposal is balanced - store this information
  balanced[i] = f.is.balanced(c(dist_x,dist_y),g,temp_dist,pop_bound,pop_ideal)
  # Removing balanced from admissible function
  admissible[i] = as.integer(contigous1 & balanced)
  Jpy[i] = f.popscore(g,temp_dist,pop_ideal,Ndist)
  Jcy[i] = f.countyscore(g,temp_dist,Mc,Ncounty)
  Jiy[i] = f.roeck(g,temp_dist,Ndist,grid)
  score_y = exp(-beta*(wp*Jpy[i]+wc*Jcy[i]+wi*Jiy[i]))
  # Calculate the acceptance function - using uniform score for now
  Qxy = f.Q(conflict_x)
  conflict_y = which(temp_p1 != temp_p2)#which(temp_p1[1:(Eint-1)] != temp_p2[1:(Eint-1)])
  Qyx = f.Q(conflict_y)
  alpha = f.alpha(Qxy,Qyx,score_x,score_y,admissible)
  # generate a uniform random variable
  U = runif(1)
  if (N == 1) {
    cat("edge =",Elist[c.edge,],"v.flip =",v.flip,"contig =",contigous1, 
        "balanced =",balanced,"admissible =",admissible, 
        "U =",round(U,3),"alpha =",round(alpha,3),sep=" ","\n")
    cat("Jpx=",round(Jpx,5),"Jcx=",round(Jcx,3),"Jix=",round(Jix,3),
        "score_x=",score_x,"\n",
        "Jpy=",round(Jpy,5),"Jcy=",round(Jcy,3),"Jiy=",round(Jiy,3),
        "score_y=",score_y,sep=" ","\n")
  }
  accepted[i] = ifelse(U <= alpha, 1,0)
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
# 6675 secs for 10000, 111 minutes, approx 2 hours

# Plot after 
# internal edges are color - conflicting edges are grey
edge_color[c.edge] = 7
frame_color = V(g)$district
frame_color[v.flip] = 7
plot(gplot,asp=1, vertex.color=vcolor[district], 
     vertex.frame.color=vcolor[frame_color],
     edge.color=vcolor[edge_color], main=paste("Run",j,sep=" "))
edge_color = ifelse(E(g)$p1 != E(g)$p2,6,E(g)$p1)
district = V(g)$district
}
library(plotrix)
par(mfrow=c(1,1),mar=c(2,2,2,0.5)+0.1)
plot(-1:12,-1:12,type="n")
d1 = data.frame(point.x = c(6,7.5,9,10.5,4.5), point.y=c(2,3,4,5,3))
d2 = data.frame(point.x = c(6,7.5,9,3,4.5), point.y=c(4,5,6,4,5))  
d3 = data.frame(point.x = c(6,7.5,1.5,3,4.5,6), point.y=c(6,7,5,6,7,8))
points(d1,pch=19,col=vcolor[1])
points(d2,pch=19,col=vcolor[2])
points(d3,pch=19,col=vcolor[3])
draw.circle(getMinCircle(d1)$ctr[1],getMinCircle(d1)$ctr[2],
            getMinCircle(d1)$rad,border=vcolor[1])
draw.circle(getMinCircle(d2)$ctr[1],getMinCircle(d2)$ctr[2],
            getMinCircle(d2)$rad,border=vcolor[2])
draw.circle(getMinCircle(d3)$ctr[1],getMinCircle(d3)$ctr[2],
            getMinCircle(d3)$rad,border=vcolor[3])
# Store info on how many admissible and how many accepted!
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



