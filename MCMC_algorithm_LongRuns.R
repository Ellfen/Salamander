# The aim of this script to find wp (the population score weighting)
######################## IMPORT FUNCTIONS AND LIBRARIES ########################
library(igraph)
library(tictoc)
source("network_functions.R")
source("rule_score_functions.R")
source("distribution_functions.R")
# Acceptance probability function f.alpha
f.alpha = function(Qxy,Qyx,score_x,score_y){
  value = min((Qyx*score_y)/(Qxy*score_x),1)
  ifelse(is.nan(value),0,value)
}
f.Q = function(conflicts){1/(2*length(conflicts))}
############################ SETUP THE NETWORK ################################
Ndist = 5
Ncounty = 38 

## CHOOSE WHICH SHAPEFILE TO READ ##
NCshp = st_read("data_cleaning/NCOriginal.shp")
#NCshp = st_read("../Data/Runs/LR/SHP/NCRejigH20K.shp")
load("../Data/Runs/LR/REAL_D3.RData")

# Create the graph
P.data = as.data.frame(NCshp)
P.data = P.data[,-dim(P.data)[2]]
adjlist = redist.adjacency(NCshp)
adjm = matrix(0, nrow=length(adjlist),ncol=length(adjlist))
for (i in 1:length(adjlist)) {
  unlist = adjlist[[i]]+1
  unlist = unlist[which(unlist>i)]
  for (j in 1:length(unlist)) {
    adjm[i,unlist[j]] = 1
  }
}
E.data = f.edges(adjm)
E.data$weight = rep(1, dim(E.data)[1])
g = graph_from_data_frame(E.data, directed = FALSE, 
                          vertices=P.data)

nodes = vcount(g)

# ABSORB DISCONTINUOUS NODES
absorb = 0
if (absorb == 1) {
  V(g)$district[which(V(g)$name=="197")] = 1
  V(g)$district[which(V(g)$name=="208")] = 1
  V(g)$district[which(V(g)$name=="221")] = 1
  V(g)$district[which(V(g)$name=="218")] = 1
}

swap = 1
if (swap == 1) {
  V(g)$district = REAL_D3$H
}

# Setup the plotting information
V(g)$size = 2 # Reduce size of the nodes
V(g)$label = NA # Do not print labels
graph_attr(g,"layout") = cbind(V(g)$centroidx,V(g)$centroidy)
vcolor = c("turquoise3","gold","olivedrab3","royalblue","darkorange","black",
           "red")
graph_attr(g,"margin") = rep(0.01,4)

# Plot and save
# CHANGE THE PLOT TITLE
par(mfrow=c(1,1),mar=c(2,2,2,0.5)+0.1)
plot(g, asp=0, vertex.color=vcolor[V(g)$district], 
     vertex.frame.color=vcolor[V(g)$district], 
     main="The Enacted Electoral Map")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")
# CHANGE THE FILE NAME
dev.copy2pdf(file="../Images/Real_init3.pdf")


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
wp = 500
wc = 2
wi = 2
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
############################## COMPACT CHECK ##################################
roeck = numeric(Ndist)
for (i in 1:Ndist) {
  xy = cbind(V(g)$centroidx[which(V(g)$district==i)],
             V(g)$centroidy[which(V(g)$district==i)])
  r = getMinCircle(xy)$rad
  Acircle = r^2*pi
  Adistrict = sum(V(g)$area[which(V(g)$district==i)])
  roeck[i] = Acircle/Adistrict
}
roeck
sum(roeck)
############################## SPLITS #########################################
o = unique(V(g)$county)
split2 = 0; split3 = 0; split4 = 0;
# get the number of counties
# Ncounty = length(unique(V(G)$county))-1
for (i in 1:Ncounty) {
  # table of how many county vertices are contained across districts
  county_districts = table(V(g)$district[which(V(g)$county==o[i])])
  # Across how many districts are the counties split
  splits = length(unique(V(g)$district[which(V(g)$county==o[i])]))
  # Loop to sum up 2- and 3-way splits and the weighting
  if (splits == 2) {
    split2 = split2+1
  } else if (splits == 3) {
    split3 = split3+1
  } else if (splits > 3) {
    split4 = split4+1
  }
}
split2
split3
split4
############################## DISTRIBUTIONS ###################################
f.seat.eff(g,Ndist)
Jp = f.popscore(g,V(g)$district,pop_ideal,Ndist)
Jc = f.countyscore(g,V(g)$district,Mc,Ncounty)$Jc
Ji = f.roeck(g,V(g)$district,Ndist)$Ji
J = (wp*Jp+wc*Jc+wi*Ji)



#set.seed(1525)
#set.seed(1527)
#set.seed(1528)
wp = 500
wi = 2
wc = 2
################################### HEATING #################################
N = 40000
beta = 0
# information to store
balanced = accepted = admissible = compact = tied = numeric(N)
J = Jiy = Jpy = Jcy = numeric(N)
redseats = effgap = numeric(N)
proposal = numeric(N)

tic()
for (i in 1:N) {
  ################################ FLIP WALK ##################################
  # step 1 - vector of conflicting edges
  conflict_x = which(E(g)$p1 != E(g)$p2) 
  # sample uniformly from all conflicting edges
  c.edge = sample(conflict_x,1)
  # choose with p=0.5 which vertex to flip (only a choice of two)
  flip = sample(1:2,1)
  # variable to store ID of vertex to be flipped
  v.flip = Elist[c.edge,flip]
  proposal[i] = v.flip
  # variables to store ID of current district and proposed district of v.flip
  dist_x = ifelse(flip==1,E(g)$p1[c.edge],E(g)$p2[c.edge])
  dist_y = ifelse(flip==1,E(g)$p2[c.edge],E(g)$p1[c.edge])
  # Create vectors to temporarily store the division information while you
  # check if the new districting obeys rules
  temp_dist = V(g)$district
  temp_p1 = E(g)$p1
  temp_p2 = E(g)$p2
  Jpx = f.popscore(g,temp_dist,pop_ideal,Ndist)
  Jcx = f.countyscore(g,temp_dist,Mc,Ncounty)$Jc
  Jix = f.roeck(g,temp_dist,Ndist)$Ji
  score_x = exp(-beta*((wp*Jpx)+(wc*Jcx)+(wi*Jix)))
  temp_dist[v.flip] = dist_y
  temp_p1[which(Elist[,1]==v.flip)] = temp_dist[v.flip]
  temp_p2[which(Elist[,2]==v.flip)] = temp_dist[v.flip]
  # Check if proposal is contigous
  contigous = f.is.contigous1(dist_x,g,temp_dist)
  # Evaluate the score functions
  Jpy[i] = f.popscore(g,temp_dist,pop_ideal,Ndist)
  c = f.countyscore(g,temp_dist,Mc,Ncounty)
  Jcy[i] = c$Jc
  roeck = f.roeck(g,temp_dist,Ndist)
  Jiy[i] = roeck$Ji
  J[i] = (wp*Jpy[i]+wc*Jcy[i]+wi*Jiy[i])
  score_y = exp(-beta*J[i])
  # Check if proposal is balanced/compact/tied - store this information
  balanced[i] = f.is.balanced2(Ndist,g,temp_dist,pop_bound,pop_ideal)
  compact[i] = roeck$compact
  tied[i] = c$tied
  d = f.seat.eff(g,Ndist)
  redseats[i] = d$seats
  effgap[i] = d$egap
  #admissible[i] = as.integer(balanced[i] & compact[i] & tied[i])
  # Calculate the acceptance function - using uniform score for now
  Qxy = f.Q(conflict_x)
  conflict_y = which(temp_p1 != temp_p2)#which(temp_p1[1:(Eint-1)] != temp_p2[1:(Eint-1)])
  Qyx = f.Q(conflict_y)
  alpha = f.alpha(Qxy,Qyx,score_x,score_y)
  # generate a uniform random variable
  U = runif(1)
  
  # if (N < 10) {
  #   cat("edge =",Elist[c.edge,],"v.flip =",v.flip,"contig =",contigous[i], 
  #       "balanced =",balanced[i],"admissible =",admissible[i], 
  #       "U =",round(U,3),"alpha =",round(alpha,3),sep=" ","\n")
  #   cat("Jpx=",round(Jpx,5),"Jcx=",round(Jcx,3),"Jix=",round(Jix,3),
  #       "score_x=",score_x,"\n",
  #       "Jpy=",round(Jpy[i],5),"Jcy=",round(Jcy[i],3),"Jiy=",round(Jiy[i],3),
  #       "score_y=",score_y,sep=" ","\n")
  # }
  
  accepted[i] = ifelse((U <= alpha) & (contigous == 1), 1,0)
  # Accept or reject proposal
  if ((U <= alpha) & (contigous == 1)){
    V(g)$district[v.flip] = dist_y
    E(g)$p1 = temp_p1
    E(g)$p2 = temp_p2
  } else {
    V(g)$district[v.flip] = dist_x
    E(g)$p1 = E(g)$p1
    E(g)$p2 = E(g)$p2
  }
  ##########################################################################
  #beta = beta + (1/(N-1))
  print(i)
}
toc()
print("Heating Finished")
# save the new district info

# save the outputs
REAL_500_2_2_H4 = data.frame("balanced"=balanced,
                                    "compact"=compact,
                                    "tied"=tied,
                                    "accepted"=accepted,
                                    "J"=J,
                                    "Jp"=Jpy,
                                    "Ji"=Jiy,
                                    "Jc"=Jcy,
                                    "proposal"=proposal,
                                    "redseats"=redseats,
                                    "effgap"=effgap)
save(REAL_500_2_2_H4,
     file="../Data/Runs/LR/REAL_500_2_2_H4.RData")
REAL_D4 = data.frame("H"=V(g)$district,"A"=rep(1,nodes),"S"=rep(1,nodes))

# CHANGE THE PLOT TITLE
par(mfrow=c(1,1),mar=c(2,2,2,0.5)+0.1)
plot(g, asp=0, vertex.color=vcolor[V(g)$district],
      vertex.frame.color=vcolor[V(g)$district],
      main="The Enacted Map After Heating")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")
# CHANGE THE FILE NAME
dev.copy2pdf(file="../Images/Real_H4.pdf")

################################### COOLING #################################
N = 60000
beta = 0
# information to store
balanced = accepted = admissible = compact = tied = numeric(N)
J = Jiy = Jpy = Jcy = numeric(N)
redseats = effgap = numeric(N)
proposal = numeric(N)

tic()
for (i in 1:N) {
  ################################ FLIP WALK ##################################
  # step 1 - vector of conflicting edges
  conflict_x = which(E(g)$p1 != E(g)$p2) 
  # sample uniformly from all conflicting edges
  c.edge = sample(conflict_x,1)
  # choose with p=0.5 which vertex to flip (only a choice of two)
  flip = sample(1:2,1)
  # variable to store ID of vertex to be flipped
  v.flip = Elist[c.edge,flip]
  proposal[i] = v.flip
  # variables to store ID of current district and proposed district of v.flip
  dist_x = ifelse(flip==1,E(g)$p1[c.edge],E(g)$p2[c.edge])
  dist_y = ifelse(flip==1,E(g)$p2[c.edge],E(g)$p1[c.edge])
  # Create vectors to temporarily store the division information while you
  # check if the new districting obeys rules
  temp_dist = V(g)$district
  temp_p1 = E(g)$p1
  temp_p2 = E(g)$p2
  Jpx = f.popscore(g,temp_dist,pop_ideal,Ndist)
  Jcx = f.countyscore(g,temp_dist,Mc,Ncounty)$Jc
  Jix = f.roeck(g,temp_dist,Ndist)$Ji
  score_x = exp(-beta*((wp*Jpx)+(wc*Jcx)+(wi*Jix)))
  temp_dist[v.flip] = dist_y
  temp_p1[which(Elist[,1]==v.flip)] = temp_dist[v.flip]
  temp_p2[which(Elist[,2]==v.flip)] = temp_dist[v.flip]
  # Check if proposal is contigous
  contigous = f.is.contigous1(dist_x,g,temp_dist)
  # Evaluate the score functions
  Jpy[i] = f.popscore(g,temp_dist,pop_ideal,Ndist)
  c = f.countyscore(g,temp_dist,Mc,Ncounty)
  Jcy[i] = c$Jc
  roeck = f.roeck(g,temp_dist,Ndist)
  Jiy[i] = roeck$Ji
  J[i] = (wp*Jpy[i]+wc*Jcy[i]+wi*Jiy[i])
  score_y = exp(-beta*J[i])
  # Check if proposal is balanced/compact/tied - store this information
  balanced[i] = f.is.balanced2(Ndist,g,temp_dist,pop_bound,pop_ideal)
  compact[i] = roeck$compact
  tied[i] = c$tied
  d = f.seat.eff(g,Ndist)
  redseats[i] = d$seats
  effgap[i] = d$egap
  #admissible[i] = as.integer(balanced[i] & compact[i] & tied[i])
  # Calculate the acceptance function - using uniform score for now
  Qxy = f.Q(conflict_x)
  conflict_y = which(temp_p1 != temp_p2)#which(temp_p1[1:(Eint-1)] != temp_p2[1:(Eint-1)])
  Qyx = f.Q(conflict_y)
  alpha = f.alpha(Qxy,Qyx,score_x,score_y)
  # generate a uniform random variable
  U = runif(1)
  
  # if (N < 10) {
  #   cat("edge =",Elist[c.edge,],"v.flip =",v.flip,"contig =",contigous[i], 
  #       "balanced =",balanced[i],"admissible =",admissible[i], 
  #       "U =",round(U,3),"alpha =",round(alpha,3),sep=" ","\n")
  #   cat("Jpx=",round(Jpx,5),"Jcx=",round(Jcx,3),"Jix=",round(Jix,3),
  #       "score_x=",score_x,"\n",
  #       "Jpy=",round(Jpy[i],5),"Jcy=",round(Jcy[i],3),"Jiy=",round(Jiy[i],3),
  #       "score_y=",score_y,sep=" ","\n")
  # }
  
  accepted[i] = ifelse((U <= alpha) & (contigous == 1), 1,0)
  # Accept or reject proposal
  if ((U <= alpha) & (contigous == 1)){
    V(g)$district[v.flip] = dist_y
    E(g)$p1 = temp_p1
    E(g)$p2 = temp_p2
  } else {
    V(g)$district[v.flip] = dist_x
    E(g)$p1 = E(g)$p1
    E(g)$p2 = E(g)$p2
  }
  ##########################################################################
  beta = beta + (1/(N-1))
  print(i)
}
toc()
print("Cooling Finished")
# save the new district info

# save the outputs
REAL_500_2_2_C4 = data.frame("balanced"=balanced,
                             "compact"=compact,
                             "tied"=tied,
                             "accepted"=accepted,
                             "J"=J,
                             "Jp"=Jpy,
                             "Ji"=Jiy,
                             "Jc"=Jcy,
                             "proposal"=proposal,
                             "redseats"=redseats,
                             "effgap"=effgap)
save(REAL_500_2_2_C4,
     file="../Data/Runs/LR/REAL_500_2_2_C4.RData")
REAL_D4$A = V(g)$district

# CHANGE THE PLOT TITLE
par(mfrow=c(1,1),mar=c(2,2,2,0.5)+0.1)
plot(g, asp=0, vertex.color=vcolor[V(g)$district],
      vertex.frame.color=vcolor[V(g)$district],
      main="The Enacted Map After Cooling")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")
# CHANGE THE FILE NAME
dev.copy2pdf(file="../Images/Real_C4.pdf")


################################## SAMPLING #################################
N = 150000
beta = 1
# information to store
balanced = accepted = admissible = compact = tied = contigous = numeric(N)
J = Jiy = Jpy = Jcy = numeric(N)
redseats = effgap = numeric(N)
proposal = numeric(N)
D = matrix(NA, nrow=nodes,ncol=0.4*N)
j=1

tic()
for (i in 1:N) {
  ################################ FLIP WALK ##################################
  # step 1 - vector of conflicting edges
  conflict_x = which(E(g)$p1 != E(g)$p2) 
  # sample uniformly from all conflicting edges
  c.edge = sample(conflict_x,1)
  # choose with p=0.5 which vertex to flip (only a choice of two)
  flip = sample(1:2,1)
  # variable to store ID of vertex to be flipped
  v.flip = Elist[c.edge,flip]
  proposal[i] = v.flip
  # variables to store ID of current district and proposed district of v.flip
  dist_x = ifelse(flip==1,E(g)$p1[c.edge],E(g)$p2[c.edge])
  dist_y = ifelse(flip==1,E(g)$p2[c.edge],E(g)$p1[c.edge])
  # Create vectors to temporarily store the division information while you
  # check if the new districting obeys rules
  temp_dist = V(g)$district
  temp_p1 = E(g)$p1
  temp_p2 = E(g)$p2
  Jpx = f.popscore(g,temp_dist,pop_ideal,Ndist)
  Jcx = f.countyscore(g,temp_dist,Mc,Ncounty)$Jc
  Jix = f.roeck(g,temp_dist,Ndist)$Ji
  score_x = exp(-beta*((wp*Jpx)+(wc*Jcx)+(wi*Jix)))
  temp_dist[v.flip] = dist_y
  temp_p1[which(Elist[,1]==v.flip)] = temp_dist[v.flip]
  temp_p2[which(Elist[,2]==v.flip)] = temp_dist[v.flip]
  # Check if proposal is contigous
  contigous[i] = f.is.contigous1(dist_x,g,temp_dist)
  # Evaluate the score functions
  Jpy[i] = f.popscore(g,temp_dist,pop_ideal,Ndist)
  c = f.countyscore(g,temp_dist,Mc,Ncounty)
  Jcy[i] = c$Jc
  roeck = f.roeck(g,temp_dist,Ndist)
  Jiy[i] = roeck$Ji
  J[i] = (wp*Jpy[i]+wc*Jcy[i]+wi*Jiy[i])
  score_y = exp(-beta*J[i])
  # Check if proposal is balanced/compact/tied - store this information
  balanced[i] = f.is.balanced2(Ndist,g,temp_dist,pop_bound,pop_ideal)
  compact[i] = roeck$compact
  tied[i] = c$tied
  d = f.seat.eff(g,Ndist)
  redseats[i] = d$seats
  effgap[i] = d$egap
  #admissible[i] = as.integer(balanced[i] & compact[i] & tied[i])
  # Calculate the acceptance function - using uniform score for now
  Qxy = f.Q(conflict_x)
  conflict_y = which(temp_p1 != temp_p2)#which(temp_p1[1:(Eint-1)] != temp_p2[1:(Eint-1)])
  Qyx = f.Q(conflict_y)
  alpha = f.alpha(Qxy,Qyx,score_x,score_y)
  # generate a uniform random variable
  U = runif(1)
  
  # if (N < 10) {
  #   cat("edge =",Elist[c.edge,],"v.flip =",v.flip,"contig =",contigous[i], 
  #       "balanced =",balanced[i],"admissible =",admissible[i], 
  #       "U =",round(U,3),"alpha =",round(alpha,3),sep=" ","\n")
  #   cat("Jpx=",round(Jpx,5),"Jcx=",round(Jcx,3),"Jix=",round(Jix,3),
  #       "score_x=",score_x,"\n",
  #       "Jpy=",round(Jpy[i],5),"Jcy=",round(Jcy[i],3),"Jiy=",round(Jiy[i],3),
  #       "score_y=",score_y,sep=" ","\n")
  # }
  
  accepted[i] = ifelse((U <= alpha) & (contigous[i] == 1), 1,0)
  # Accept or reject proposal
  if ((U <= alpha) & (contigous[i] == 1)){
    V(g)$district[v.flip] = dist_y
    E(g)$p1 = temp_p1
    E(g)$p2 = temp_p2
    D[,j] = V(g)$district
    j=j+1
  } else {
    V(g)$district[v.flip] = dist_x
    E(g)$p1 = E(g)$p1
    E(g)$p2 = E(g)$p2
  }
  ##########################################################################
  #beta = beta + (1/(N-1))
  print(i)
}
toc()
print("Sampling Finished")
# save the new district info

# save the outputs
REAL_500_2_2_S4 = data.frame("balanced"=balanced,
                             "compact"=compact,
                             "tied"=tied,
                             "accepted"=accepted,
                             "J"=J,
                             "Jp"=Jpy,
                             "Ji"=Jiy,
                             "Jc"=Jcy,
                             "proposal"=proposal,
                             "redseats"=redseats,
                             "effgap"=effgap)
save(REAL_500_2_2_S4,
     file="../Data/Runs/LR/REAL_500_2_2_S4.RData")

REAL_D4$S = V(g)$district

save(REAL_D4, file="../Data/Runs/LR/REAL_D4.RData")
save(D4, file="../Data/Runs/LR/matrixD4.RData")

############################ PRELIMINARY ANALYSIS ############################

# Plot after 
par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1)
plot(g,asp=0, vertex.color=vcolor[(V(g)$district)], 
     vertex.frame.color=vcolor[V(g)$district], 
     main="A Reasonable Redistrict Example")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")
dev.copy2pdf(file="../Images/Real_end3.pdf")

mean(J)
plot(J,type="l",main="The score function, J, of the proposals")
hist(J)
dev.copy2pdf(file="../Images/Real_J3.pdf")

a = 1
b = 4915

unique_proposals = unique(proposal[a:b])
par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1)
hist(proposal[a:b],breaks = length(unique_proposals),main="Histogram of proposals")
dev.copy2pdf(file="../Images/Real_proposals3.pdf")

vertex_color = V(g)$district
vertex_color[unique_proposals] = 6
par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1)
plot(g,asp=0, vertex.color=vcolor[V(g)$district], 
     vertex.frame.color=vcolor[vertex_color],
     main="2011 Map Proposed Vertices")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5","Proposed Vertices"),
       col=vcolor[1:6],pch=c(rep(19,5),1))#,bty="n")

# Store info on how many admissible, how many balanced, and how many accepted
# When beta = 0 then mean(admissible) approx mean(accepted)
mean(balanced[a:b])
mean(tied[a:b])
min(compact[a:b])
max(compact[a:b])
length(compact[which(compact[a:b] <= 7.5)])/N
min(Jiy)
max(Jiy)
length(Jiy[which(Jiy[a:b] <= 19)])/N
admissible = as.integer(balanced[a:b]&tied[a:b])
mean(admissible)
mean(accepted[a:b])

outcome = data.frame("accepted"=accepted[a:b],"admissible"=admissible[a:b],
                     "redseats"=redseats[a:b], "effgap"=effgap[a:b], 
                     "proposals" = proposal[a:b], "Jp"=Jpx, "Jy"=Jiy,"Jc"=Jcy,
                     "J"=J,"compact"=compact)
outcome = outcome[-which(outcome$accepted==0),]
unique_proposals2 = unique(outcome$proposals)
min(which(is.na(D[1,])))
Dclean = D
Dclean = t(Dclean)
dim(Dclean)
Dclean = Dclean[-which(outcome$admissible==0),]
dim(Dclean)
outcome = outcome[-which(outcome$admissible==0),]
outcome$blueseats = Ndist - outcome$redseats
#add a row for the enacted map

par(mfrow=c(1,1),mar=c(3,3,3,3)+0.1)
hist(outcome$redseats, freq=F,
     breaks=seq(min(outcome$redseats)-0.5, max(outcome$redseats)+0.5, by=1),
     main="Histogram of Seats",xlim=c(1.5,3.5),col="red3")
hist(outcome$blueseats, freq=F, add=T, 
     breaks=seq(min(outcome$blueseats)-0.5, max(outcome$blueseats)+0.5, by=1),
     col="blue3")
legend("center",legend=c("Democratic Seats","Republican Seats"), 
       col=c("blue3","red3"),pch=15)
dev.copy2pdf(file="../Images/Real_3_seats.pdf")
#hist(outcome$blueseats, freq=F,
     #breaks=seq(min(outcome$blueseats)-0.5, max(outcome$blueseats)+0.5, by=1))
#par(mfrow=c(1,1),mar=c(3,3,3,3)+0.1)
par(mfrow=c(1,2),mar=c(3,3,3,3)+0.1)
hist(outcome$effgap,freq=F,breaks=20, main="Histogram of Efficiency Gap",
     col="seagreen")
hist(outcome$J, freq=F, breaks=20, main="Histrogram of Score Function",
     col="seagreen")
dev.copy2pdf(file="../Images/Real_3_egap_score.pdf")


vertex_color = V(g)$district
vertex_color[unique_proposals2] = 6
par(mfrow=c(1,1),mar=c(3,3,3,3)+0.1)
plot(g,asp=0, vertex.color=vcolor[V(g)$district], 
     vertex.frame.color=vcolor[vertex_color],
     main="Accepted Boundary Flips")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5","Accepted Vertices"),
       col=vcolor[1:6],pch=c(rep(19,5),1))#,bty="n")
dev.copy2pdf(file="../Images/Real_3_acceptedflips.pdf")

par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1)
hist(outcome$proposals,breaks = length(unique_proposals2),
     main="Histogram of Accepted proposals")
dev.copy2pdf(file="../Images/Real_accproposals3.pdf")

par(mfrow=c(1,1),mar=c(3,3,3,3)+0.1)
dim(Dclean)
rsample = sample(1:dim(Dclean)[1],1)
for(i in 1:15) {
  plot(g,asp=0, vertex.color=vcolor[Dclean[rsample,]], 
       vertex.frame.color=vcolor[Dclean[rsample,]], 
       main="A Reasonable Redistrict Example")
  legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                            "District 5"),
         col=vcolor[1:5],pch=19)#,bty="n")
}
dev.copy2pdf(file="../Images/Real_3_example.pdf")

rsample = sample(1:dim(Dclean)[1],1)
V(g)$district = t(Dclean[rsample,])
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
############################## COMPACT CHECK ##################################
roeck = numeric(Ndist)
for (i in 1:Ndist) {
  xy = cbind(V(g)$centroidx[which(V(g)$district==i)],
             V(g)$centroidy[which(V(g)$district==i)])
  r = getMinCircle(xy)$rad
  Acircle = r^2*pi
  Adistrict = sum(V(g)$area[which(V(g)$district==i)])
  roeck[i] = Acircle/Adistrict
}
roeck
sum(roeck)
############################## SPLITS #########################################
o = unique(V(g)$county)
split2 = 0; split3 = 0; split4 = 0;
# get the number of counties
# Ncounty = length(unique(V(G)$county))-1
for (i in 1:Ncounty) {
  # table of how many county vertices are contained across districts
  county_districts = table(V(g)$district[which(V(g)$county==o[i])])
  # Across how many districts are the counties split
  splits = length(unique(V(g)$district[which(V(g)$county==o[i])]))
  # Loop to sum up 2- and 3-way splits and the weighting
  if (splits == 2) {
    split2 = split2+1
  } else if (splits == 3) {
    split3 = split3+1
  } else if (splits > 3) {
    split4 = split4+1
  }
}
split2
split3
split4
############################## DISTRIBUTIONS ###################################
f.seat.eff(g,Ndist)
Jp = f.popscore(g,V(g)$district,pop_ideal,Ndist)
Jc = f.countyscore(g,V(g)$district,Mc,Ncounty)$Jc
Ji = f.roeck(g,V(g)$district,Ndist)$Ji
J = (wp*Jp+wc*Jc+wi*Ji)


