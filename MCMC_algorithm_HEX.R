# The aim of this script to find wp (the population score weighting)
######################## IMPORT FUNCTIONS AND LIBRARIES ########################
library(igraph)
library(tictoc)
source("network_functions.R")
source("rule_score_functions.R")
source("distribution_functions.R")
# Acceptance probability function f.alpha
f.alpha = function(Qxy,Qyx,score_x,score_y,admissible){
  min((Qyx*score_y)/(Qxy*score_x), 1)
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
g = f.perimeter(gplot,nodes)

# Absorb discontiguous nodes
if (grid == 5) {
  V(g)$district[which(V(g)$name=="197")] = 1
  V(g)$district[which(V(g)$name=="208")] = 1
  V(g)$district[which(V(g)$name=="221")] = 1
  V(g)$district[which(V(g)$name=="218")] = 1
}

# Setup the plotting information
V(gplot)$size = 15
V(gplot)$label = V(gplot)$name#V(gplot)$county
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
par(mar=c(2,0,2,0)+0.1,mfrow=c(1,1),cex=1.5)
plot(gplot, vertex.color=vcolor[get.vertex.attribute(g,"district")], 
     vertex.frame.color=vcolor[get.vertex.attribute(g,"district")],
     asp=1)

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
edge_color = ifelse(E(g)$p1[1:(Eint-1)] != E(g)$p2[1:Eint-1],6,E(g)$p1[1:(Eint-1)])#ifelse(E(g)$p1 != E(g)$p2,6,E(g)$p1)
district = V(g)$district
#dev.new()
par(mfrow=c(1,1),mar=c(2,1.3,2,0.5)+0.1,cex=2)
plot(gplot, asp=1, vertex.color=vcolor[district], 
     vertex.frame.color=vcolor[district],
     edge.color=vcolor[edge_color], main="Initial Districting")
legend("topleft",legend=c("District 1","District 2", "District 3"),
       col=vcolor[1:3],pch=19,bty="n")
#dev.copy2pdf(file="../Images/hex_init.pdf")
#dev.off(); graphics.off()

#axis(1)
#axis(2,pos=-1.1)
#dev.new()
par(mfrow=c(1,3),mar=c(2,1.3,2,0.5)+0.1,cex=1.8)
a = c(1525,1527,1528)
for (j in 1:3) {
set.seed(a[j])
# Boundary flip in a loop
N = 50
# information to store
balanced = accepted = numeric(N)
Jpx = Jpy = Jcx = Jcy = Jix = Jiy = effgap = seats = polsbyx = polsbyy = numeric(N)
tic()
for (i in 1:N) {
  print(i)
  ################################ FLIP WALK ##################################
  # step 1 - vector of conflicting edges
  conflict_x = which(E(g)$p1[1:(Eint-1)] != E(g)$p2[1:Eint-1]) #which(E(g)$p1 != E(g)$p2)
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
  Jcx[i] = f.countyscore(g,temp_dist,Mc,Ncounty)$Jc
  Jix[i] = f.roeck(g,temp_dist,Ndist)$Ji
  polsbyx[i] = f.compactscore(g,temp_dist,temp_p1,temp_p2,Ndist)
  Jx = ((wp*Jpx[i])+(wc*Jcx[i])+(wi*Jix[i]))
  score_x = exp(-beta*((wp*Jpx[i])+(wc*Jcx[i])+(wi*Jix[i])))
  temp_dist[v.flip] = dist_y
  temp_p1[which(Elist[,1]==v.flip)] = temp_dist[v.flip]
  temp_p2[which(Elist[,2]==v.flip)] = temp_dist[v.flip]
  # Check if proposal is contigous
  contigous1 = f.is.contigous1(dist_x,g,temp_dist)
  # Check if proposal is balanced - store this information
  balanced[i] = f.is.balanced2(Ndist,g,temp_dist,pop_bound,pop_ideal)
  Jpy[i] = f.popscore(g,temp_dist,pop_ideal,Ndist)
  Jcy[i] = f.countyscore(g,temp_dist,Mc,Ncounty)$Jc
  Jiy[i] = f.roeck(g,temp_dist,Ndist)$Ji
  polsbyy[i] = f.compactscore(g,temp_dist,temp_p1,temp_p2,Ndist)
  Jy = (wp*Jpy[i]+wc*Jcy[i]+wi*Jiy[i])
  deltaJ = Jy - Jx
  out = exp(-beta*deltaJ)
  score_y = exp(-beta*(wp*Jpy[i]+wc*Jcy[i]+wi*Jiy[i]))
  # Calculate the acceptance function - using uniform score for now
  Qxy = f.Q(conflict_x)
  conflict_y = which(temp_p1 != temp_p2)#which(temp_p1[1:(Eint-1)] != temp_p2[1:(Eint-1)])
  Qyx = f.Q(conflict_y)
  alpha = f.alpha(Qxy,Qyx,score_x,score_y)
  # generate a uniform random variable
  U = runif(1)
  if (N == 1) {
    cat("edge =",Elist[c.edge,],"v.flip =",v.flip,"contig =",contigous1, 
        "balanced =",balanced, 
        "U =",round(U,3),"alpha =",round(alpha,3),sep=" ","\n")
    cat("Jpx=",round(Jpx,5),"Jcx=",round(Jcx,3),"Jix=",round(Jix,3),
        "PPx=",polsbyx,
        "score_x=",score_x,"\n",
        "Jpy=",round(Jpy,5),"Jcy=",round(Jcy,3),"Jiy=",round(Jiy,3),
        "PPy=",polsbyy,
        "score_y=",score_y,sep=" ","\n")
    cat("Jx=",round(Jx,5),"Jy=",round(Jy,3),"deltaJ=",Jy-Jx, "out=",round(out,4),sep=" ","\n")
  }
  accepted[i] = ifelse(U <= alpha & contigous1==1 , 1,0)
  # Accept or reject proposal
  if (U <= alpha & contigous1 == 1){
    V(g)$district[v.flip] = dist_y
    E(g)$p1 = temp_p1
    E(g)$p2 = temp_p2
  } else {
    V(g)$district[v.flip] = dist_x
    E(g)$p1 = E(g)$p1
    E(g)$p2 = E(g)$p2
  }
  ##########################################################################
  effgap[i] = f.seat.eff(g,Ndist)$egap
  seats[i] = f.seat.eff(g,Ndist)$seats
  # print(effgap)
  # print(seats)
}
toc()
# 6675 secs for 10000, 111 minutes, approx 2 hours

# Plot after 
# internal edges are color - conflicting edges are grey
# edge_color[c.edge] = 7
# frame_color = V(g)$district
# frame_color[v.flip] = 7
# plot(gplot,asp=1, vertex.color=vcolpior[district], 
#      vertex.frame.color=vcolor[frame_color],
#      edge.color=vcolor[edge_color], main=paste("Proposal",j,sep=" "))
# edge_color = ifelse(E(g)$p1 != E(g)$p2,6,E(g)$p1)
# district = V(g)$district
}
# dev.copy2pdf(file="../Images/hex_4.pdf")
# dev.off()
par(mfrow=c(1,3),mar=c(3,3,3,3)+0.1,cex=1.3,cex.main=1.1)
plot(gplot, asp=1, vertex.color=vcolor[district], 
     vertex.frame.color=vcolor[district],
     edge.color=vcolor[edge_color], main="VTD Nodes")
par(cex=1,cex.main=1.1)
hist(seats[which(accepted==1)], freq=F,
     breaks=seq(min(seats)-0.5, max(seats)+0.5, by=1),
     main="Histogram of Seats",xlim=c(-0.5,3.5),ylim=c(0,1),col="red3")
hist(3-seats[which(accepted==1)], freq=F, add=T, 
     breaks=seq(min(3-seats)-0.5, max(3-seats)+0.5, by=1),
     col="blue3")
legend("center",legend=c("Blue Seats","Red Seats"), 
       col=c("blue3","red3"),pch=15)
par(cex=1.1)
hist(effgap[which(accepted==1)],freq=F,ylim=c(0,100),breaks=20, main="Histogram of Efficiency Gap",
     col="seagreen")
dev.copy2pdf(file="../Images/hex_egap_seats.pdf")
hist(polsbyx[which(accepted==1)])
hist(Jix[which(accepted==1)])

par(mfrow=c(1,2),mar=c(2,1.3,2,0.5)+0.1)
plot(gplot, asp=1, vertex.color="white", 
     vertex.frame.color="blue",
     edge.color=vcolor[edge_color], main="Initial Districting",
     vertex.label=V(g)$blue)
plot(gplot, asp=1, vertex.color="white", 
     vertex.frame.color="red",
     edge.color=vcolor[edge_color], main="Initial Districting",
     vertex.label=V(g)$red)
legend("topleft",legend=c("District 1","District 2", "District 3"),
       col=vcolor[1:3],pch=19,bty="n")

library(plotrix)
library(shotGroups)
par(mfrow=c(1,1),mar=c(2,2,2,0.5)+0.1)
plot(-1:12,-1:12,type="n")
d1 = data.frame(point.x = c(6,7.5,9,10.5,4.5), point.y=c(0,0.87,1.73,2.6,0.87))
d2 = data.frame(point.x = c(6,7.5,9,3,4.5,1.5), point.y=c(1.73,2.6,3.46,1.73,2.6,2.6))  
d3 = data.frame(point.x = c(6,7.5,3,4.5,6), point.y=c(3.46,4.33,3.46,4.33,5.17))
points(d1,pch=19,col=vcolor[1])
points(d2,pch=19,col=vcolor[2])
points(d3,pch=19,col=vcolor[3])
draw.circle(getMinCircle(d1)$ctr[1],getMinCircle(d1)$ctr[2],
            getMinCircle(d1)$rad,border=vcolor[1])
draw.circle(getMinCircle(d2)$ctr[1],getMinCircle(d2)$ctr[2],
            getMinCircle(d2)$rad,border=vcolor[2])
draw.circle(getMinCircle(d3)$ctr[1],getMinCircle(d3)$ctr[2],
            getMinCircle(d3)$rad,border=vcolor[3])
getMinCircle(d1)$rad
getMinCircle(d2)$rad
getMinCircle(d3)$rad

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

png("grid.png")

dev.off(); graphics.off()

