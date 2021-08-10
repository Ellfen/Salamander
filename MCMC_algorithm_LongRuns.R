# The aim of this script to find wp (the population score weighting)
######################## IMPORT FUNCTIONS AND LIBRARIES ########################
library(igraph)
library(tictoc)
source("network_functions.R")
source("rule_score_functions.R")
source("distribution_functions.R")
# Acceptance probability function f.alpha
f.alpha = function(Qxy,Qyx,score_x,score_y,admissible){
  value = min((Qyx*score_y*admissible)/(Qxy*score_x),1)
  ifelse(is.nan(value),0,value)
}
f.Q = function(conflicts){1/(2*length(conflicts))}
############################ SETUP THE NETWORK ################################
Ndist = 5
Ncounty = 38 

## CHOOSE WHICH SHAPEFILE TO READ ##
NCshp = st_read("data_cleaning/NCRejig.shp")

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
yes = 0
if (yes == 1) {
  V(g)$district[which(V(g)$name=="197")] = 1
  V(g)$district[which(V(g)$name=="208")] = 1
  V(g)$district[which(V(g)$name=="221")] = 1
  V(g)$district[which(V(g)$name=="218")] = 1
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
     main="Alternative Initial District Assignment")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")
# CHANGE THE FILE NAME
dev.copy2pdf(file="../Images/Alternative_init.pdf")


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

#set.seed(1525)
#set.seed(1527)
#set.seed(1528)
wp = 500
wi = 2
wc = 2
################################### HEATING #################################
N = 20e3
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
  balanced[i] = f.is.balanced(c(dist_x,dist_y),g,temp_dist,pop_bound,pop_ideal)
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
  alpha = f.alpha(Qxy,Qyx,score_x,score_y,contigous)
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
  #beta = beta + (1/(N-1))
}
toc()
print("Heating Finished")
# save the new district info

# save the outputs
REJIG_500_2_2_H = data.frame("balanced"=balanced,
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
save(REJIG_500_2_2_H,
     file="../Data/Runs/LR/REJIG_500_2_2_H.RData")
REJIG_D = data.frame("H"=V(g)$district,"A"=rep(1,nodes),"S"=rep(1,nodes))


################################### COOLING #################################
N = 30e3
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
  balanced[i] = f.is.balanced(c(dist_x,dist_y),g,temp_dist,pop_bound,pop_ideal)
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
  alpha = f.alpha(Qxy,Qyx,score_x,score_y,contigous)
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
  beta = beta + (1/(N-1))
}
toc()
print("Cooling Finished")
# save the new district info

# save the outputs
REJIG_500_2_2_C = data.frame("balanced"=balanced,
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
save(REJIG_500_2_2_C,
     file="../Data/Runs/LR/REJIG_500_2_2_C.RData")
REJIG_D$A = V(g)$district

################################## SAMPLING #################################
N = 100e3
beta = 1
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
  balanced[i] = f.is.balanced(c(dist_x,dist_y),g,temp_dist,pop_bound,pop_ideal)
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
  alpha = f.alpha(Qxy,Qyx,score_x,score_y,contigous)
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
  #beta = beta + (1/(N-1))
}
toc()
print("Sampling Finished")
# save the new district info

# save the outputs
REJIG_500_2_2_S = data.frame("balanced"=balanced,
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
save(REJIG_500_2_2_S,
     file="../Data/Runs/LR/REJIG_500_2_2_S.RData")
REJIG_D$S = V(g)$district

save(REJIG_D, file="../Data/Runs/LR/REJIG_D.RData")

############################ PRELIMINARY ANALYSIS ############################

unique_proposals = length(unique(proposal))
length(which(E(g)$p1 != E(g)$p2))
par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1)
hist(proposal,breaks = unique_proposals)

# Plot after 
par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1)
plot(g,asp=0, vertex.color=vcolor[(V(g)$district)], 
     vertex.frame.color=vcolor[V(g)$district], main="15K Steps")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")

mean(J)
plot(J,type="l")

# Store info on how many admissible, how many balanced, and how many accepted
# When beta = 0 then mean(admissible) approx mean(accepted)
mean(balanced)
mean(compact)
min(compact)
max(compact)
mean(tied)
admissible = as.integer(balanced&tied)
mean(admissible)
mean(accepted)

mean(Jiy)
min(Jiy)
max(Jiy)
length(Jiy[which(Jiy <= 20)])/N

outcome = data.frame("accepted"=accepted,"admissible"=admissible,
                     "redseats"=redseats)
outcome = outcome[-which(outcome$accepted==0),]
outcome$blueseats = Ndist - outcome$redseats

par(mfrow=c(1,2),mar=c(3,3,3,3)+0.1)
hist(outcome$redseats, freq=F,
     breaks=seq(min(outcome$redseats)-0.5, max(outcome$redseats)+0.5, by=1))
#red_seats
hist(outcome$blueseats, freq=F,
     breaks=seq(min(outcome$blueseats)-0.5, max(outcome$blueseats)+0.5, by=1))
hist(effgap)
