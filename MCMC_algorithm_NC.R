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
grid = 5
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

vcolor = c("turquoise3","gold","olivedrab3","royalblue","darkorange","black","red")
#vshape = c("circle","square","rectangle","none")



graph_attr(gplot,"margin") = rep(0.01,4)
par(mar=c(2,0,2,0)+0.1,mfrow=c(1,1))
plot(gplot, vertex.color=vcolor[get.vertex.attribute(g,"district")], 
     vertex.frame.color=vcolor[get.vertex.attribute(g,"district")],
     asp=0)
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
deviation = 0.01
ubound = pop_ideal+(deviation*pop_ideal/2)
lbound = pop_ideal-(deviation*pop_ideal/2)
pop_bound = c(lbound,ubound); pop_bound
# county split constants
Mc = 100
# score function weightings
wp = 0
wc = 0
wi = 0
# temperature
beta = 0
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
#edge_color = ifelse(E(g)$p1 != E(g)$p2,6,E(g)$p1)
district = V(g)$district
par(mfrow=c(1,1),mar=c(2,2,2,0.5)+0.1)
plot(gplot, asp=0, vertex.color=vcolor[district], 
     vertex.frame.color=vcolor[district], main="Initial Districting")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")

# set.seed(1412)
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
  admissible[i] = as.integer(contigous1) #& balanced[i])
  Jpy[i] = f.popscore(g,temp_dist,pop_ideal,Ndist)
  Jcy[i] = f.countyscore(g,temp_dist,Mc,Ncounty)
  Jiy[i] = f.roeck(g,temp_dist,Ndist,grid)
  score_y = exp(-beta*(wp*Jpy[i]+wc*Jcy[i]+wi*Jiy[i]))
  # Calculate the acceptance function - using uniform score for now
  Qxy = f.Q(conflict_x)
  conflict_y = which(temp_p1 != temp_p2)#which(temp_p1[1:(Eint-1)] != temp_p2[1:(Eint-1)])
  Qyx = f.Q(conflict_y)
  alpha = f.alpha(Qxy,Qyx,score_x,score_y,admissible[i])
  # generate a uniform random variable
  U = runif(1)
  if (N <= 10) {
    cat("edge =",Elist[c.edge,],"v.flip =",v.flip,"contig =",contigous1, 
        "balanced =",balanced[i],"admissible =",admissible[i], 
        "U =",round(U,3),"alpha =",round(alpha,3),sep=" ","\n")
    cat("Jpx=",round(Jpx[i],5),"Jcx=",round(Jcx[i],3),"Jix=",round(Jix[i],3),
        "score_x=",score_x,"\n",
        "Jpy=",round(Jpy[i],5),"Jcy=",round(Jcy[i],3),"Jiy=",round(Jiy[i],3),
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
# 6675 secs for 10000, 111 minutes, approx 2 hours - OLD CODE
# 35 secs for 400 therefore 3500 secs for 40000 - 1 hour

# Plot after 
plot(gplot,asp=0, vertex.color=vcolor[V(g)$district], 
     vertex.frame.color=vcolor[V(g)$district], main="40,000 Steps")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")

# Store info on how many admissible, how many balanced, and how many accepted
# When beta = 0 then mean(admissible) approx mean(accepted)
balanced
mean(balanced)
admissible
mean(admissible)
accepted
mean(accepted)

mean(Jpx)
mean(Jpy)
mean(Jcx)
mean(Jcy)
mean(Jix)
mean(Jiy)


# png("grid.png")
# plot(gplot, vertex.color=V(g)$district, vertex.frame.color=V(g)$district)
# dev.off(); graphics.off()



