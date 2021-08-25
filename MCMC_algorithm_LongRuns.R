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
#NCshp = st_read("data_cleaning/NCOriginal.shp")
NCshp = st_read("data_cleaning/NCRejig2.shp")
load("../Data/Runs/LR/PERTURB_1.RData")

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
if (absorb == 2) {
  #absorb yellow block into 5
  names = V(g)$name
  class(names) = "numeric"
  index = names[which(V(g)$district==2)]
  index = index[which(index >= 275 & index <= 386)]
  V(g)$district[index] = 5
  # absorb lost blues into 5
  V(g)$district[278]=5
  V(g)$district[380]=5
  V(g)$district[325]=5
  # absorb lost orange into purple
  V(g)$district[882]=4
  V(g)$district[986]=4
  V(g)$district[867]=4
  V(g)$district[969]=4
  V(g)$district[983]=4
  V(g)$district[989]=4
}
for (i in 1:5) {
  print(sum(V(g)$district==i))
}

swap = 1
if (swap == 1) {
  V(g)$district = PERTURB_1$S
}

# Setup the plotting information
V(g)$size = 2 # Reduce size of the nodes
V(g)$label = NA # Do not print labels
graph_attr(g,"layout") = cbind(V(g)$centroidx,V(g)$centroidy)
vcolor = c("turquoise3","gold","olivedrab3","royalblue","darkorange","black",
           "red")
graph_attr(g,"margin") = rep(0.01,4)

# The edgelist won't change so create it now
Elist = get.edgelist(g)
class(Elist) = "numeric"
E(g)$p1 = V(g)$district[Elist[,1]]
E(g)$p2 = V(g)$district[Elist[,2]]
# Make a note of which edges are perimeter edges
# Eint = min(which(Elist[,2]==nodes+1))

#V(g)$label=rep(NA,1060)
#V(g)$label[which(V(g)$district==5)] = V(g)$name[which(V(g)$district==5)]
#V(g)$label=NA
# Plot and save
# CHANGE THE PLOT TITLE
par(mfrow=c(1,1),mar=c(2,2,2,0.5)+0.1)
plot(g, asp=0, vertex.color=vcolor[V(g)$district], 
     vertex.frame.color=vcolor[V(g)$district], 
     main="Perturbed Starting Configuration A")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")
# CHANGE THE FILE NAME
dev.copy2pdf(file="../Images/Perturb_init1.pdf")

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
  #print(i)
}
toc()
print("Heating Finished")
# save the new district info

# save the outputs
PERTURB_500_2_2_H1 = data.frame("balanced"=balanced,
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
save(PERTURB_500_2_2_H1,
     file="../Data/Runs/LR/PERTURB_500_2_2_H1.RData")
PERTURB_2 = data.frame("H"=V(g)$district,"A"=rep(1,nodes),"S"=rep(1,nodes))

# CHANGE THE PLOT TITLE
par(mfrow=c(1,1),mar=c(2,2,2,0.5)+0.1)
plot(g, asp=0, vertex.color=vcolor[V(g)$district],
      vertex.frame.color=vcolor[V(g)$district],
      main="Perturbation A After Heating")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")
# CHANGE THE FILE NAME
dev.copy2pdf(file="../Images/PERTURB_1_H.pdf")

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
  #print(i)
}
toc()
print("Cooling Finished")
# save the new district info

# save the outputs
PERTURB_500_2_2_C1 = data.frame("balanced"=balanced,
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
save(PERTURB_500_2_2_C1,
     file="../Data/Runs/LR/PERTURB_500_2_2_C1.RData")
PERTURB_1$A = V(g)$district

# CHANGE THE PLOT TITLE
par(mfrow=c(1,1),mar=c(2,2,2,0.5)+0.1,cex=1.1)
plot(g, asp=0, vertex.color=vcolor[V(g)$district],
      vertex.frame.color=vcolor[V(g)$district],
      main="Perturbation A After Cooling")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")
# CHANGE THE FILE NAME
dev.copy2pdf(file="../Images/PERTURB_1_C.pdf")


################################## SAMPLING #################################
N = 100000
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
PERTURB_500_2_2_S2 = data.frame("balanced"=balanced,
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
save(PERTURB_500_2_2_S2,
     file="../Data/Runs/LR/PERTURB_500_2_2_S2.RData")

PERTURB_2$S = V(g)$district

save(PERTURB_2, file="../Data/Runs/LR/PERTURB_2.RData")
save(D, file="../Data/Runs/LR/matrix_perturb2.RData")

############################ PRELIMINARY ANALYSIS ############################
# Plot after 
par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1,cex=1.1)
plot(g,asp=0, vertex.color=vcolor[(V(g)$district)], 
     vertex.frame.color=vcolor[V(g)$district], 
     main="A Reasonable Redistrict Example")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")
dev.copy2pdf(file="../Images/PERTURB_1_END2.pdf")

a = 1
b = i

mean(J[a:b])
plot(J[a:b],type="l",main="The score function, J, of the proposals")
#hist(J[a:b])
dev.copy2pdf(file="../Images/PERTURB_1_J1.pdf")

a = 1
b = 89678
plot(J[a:b],type="l",main="The score function, J, of the proposals")


unique_proposals = unique(proposal[a:b])
par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1)
hist(proposal[a:b],breaks = length(unique_proposals),main="Histogram of proposals")
dev.copy2pdf(file="../Images/PERTURB_1_proposals1.pdf")

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
mean(balanced[a:b])
mean(tied[a:b])
min(compact[a:b])
max(compact[a:b])
length(compact[which(compact[a:b] <= 6)])/(b-a)
min(Jiy[a:b])
max(Jiy[a:b])
length(Jiy[which(Jiy[a:b] <= 18)])/(b-a)
admissible = as.integer(balanced[a:b]&tied[a:b])
adm = as.integer(balanced[1:b]&tied[1:b])[which(accepted[1:b]==1)]
mean(admissible)
mean(accepted[a:b])

outcome = data.frame("accepted"=accepted[a:b],"admissible"=admissible[a:b],
                     "redseats"=redseats[a:b], "effgap"=effgap[a:b], 
                     "proposals" = proposal[a:b], "Jp"=Jpx[a:b], 
                     "Jy"=Jiy[a:b],"Jc"=Jcy[a:b],
                     "J"=J[a:b],"compact"=compact[a:b])
outcome = outcome[-which(outcome$accepted==0),]
unique_proposals2 = unique(outcome$proposals)
min(which(is.na(D[2,])))
Dclean = D[,1:(min(which(is.na(D[1,])))-1)]
dim(Dclean)
which(is.na(Dclean[1,]))
Dclean = t(Dclean)
dim(Dclean)
Dclean = Dclean[which(adm==1),]
dim(Dclean)
outcome = outcome[-which(outcome$admissible==0),]
outcome$blueseats = Ndist - outcome$redseats
#add a row for the enacted map

par(mfrow=c(1,1),mar=c(3,3,3,3)+0.1,cex=1.1)
hist(outcome$redseats, freq=F,
     breaks=seq(min(outcome$redseats)-0.5, max(outcome$redseats)+0.5, by=1),
     main="Histogram of Seats",xlim=c(1.5,3.5),col="red3")
hist(outcome$blueseats, freq=F, add=T, 
     breaks=seq(min(outcome$blueseats)-0.5, max(outcome$blueseats)+0.5, by=1),
     col="blue3")
legend("center",legend=c("Blue Seats","Red Seats"), 
       col=c("blue3","red3"),pch=15)
dev.copy2pdf(file="../Images/PERTURB_1_seats1.pdf")
#hist(outcome$blueseats, freq=F,
     #breaks=seq(min(outcome$blueseats)-0.5, max(outcome$blueseats)+0.5, by=1))
#par(mfrow=c(1,1),mar=c(3,3,3,3)+0.1)
par(mfrow=c(1,2),mar=c(3,3,3,3)+0.1,cex=1.1)
hist(outcome$effgap,freq=F,breaks=20, main="Histogram of Efficiency Gap",
     col="seagreen")
hist(outcome$Jy, freq=F, breaks=20, main="Histrogram of Roeck Score",
     col="seagreen")
dev.copy2pdf(file="../Images/PERTURB_1_egap_roeck1.pdf")
mean(outcome$effgap)
mean(outcome$Jy)
mean(outcome$compact)
mean(outcome$effgap >= 0.32)
mean(outcome$Jy >= 25.806)

# Plot an example of a reasonable redistricting
par(mfrow=c(1,1),mar=c(3,3,3,3)+0.1)
dim(Dclean)
rsample = sample(1:dim(Dclean)[1],1)
plot(g,asp=0, vertex.color=vcolor[Dclean[rsample,]], 
     vertex.frame.color=vcolor[Dclean[rsample,]], 
     main="A Reasonable Redistrict Example")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5"),
       col=vcolor[1:5],pch=19)#,bty="n")
dev.copy2pdf(file="../Images/PERTURB_1_example6097.pdf")

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

vertex_color = V(g)$district
vertex_color[unique_proposals2] = 6
par(mfrow=c(1,1),mar=c(3,3,3,3)+0.1,cex=1.1)
plot(g,asp=0, vertex.color=vcolor[V(g)$district], 
     vertex.frame.color=vcolor[vertex_color],
     main="Accepted Boundary Flips")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5","Accepted Vertices"),
       col=vcolor[1:6],pch=c(rep(19,5),1))#,bty="n")
dev.copy2pdf(file="../Images/PERTURB_1_acceptedflips1.pdf")

par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1,cex=1.1)
hist(outcome$proposals,breaks = 1060,ylim=c(0,100),
     main="Histogram of Accepted Proposals")
dev.copy2pdf(file="../Images/PERTURB_1_accproposals1.pdf")

unique_proposals = unique(proposal[a:b])
par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1,cex=1.1)
hist(proposal[a:b],breaks = 1060,
     main="Histogram of All Proposals")
dev.copy2pdf(file="../Images/PERTURB_1_proposals1.pdf")

vertex_color = V(g)$district
vertex_color[unique_proposals] = 6
par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1,cex=1.1)
plot(g,asp=0, vertex.color=vcolor[V(g)$district], 
     vertex.frame.color=vcolor[vertex_color],
     main="Proposed Boundary Flips")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5","Proposed Vertices"),
       col=vcolor[1:6],pch=c(rep(19,5),1))#,bty="n")

dev.copy2pdf(file="../Images/PERTURB_1_proposedflips1.pdf")

# apply Monte Carlo to get expected values
mean(outcome$effgap)
mean(outcome$Jy)
mean(outcome$compact)

total=matrix(NA,nrow=5,ncol=2)
seats=numeric(Ndist)
for (i in 1:Ndist) {
  # total democrate vote
  total[i,1] = sum(vertex_attr(g,"blue")[which(V(g)$district == i)])
  # total republican vote
  total[i,2] = sum(vertex_attr(g,"red")[which(V(g)$district == i)])
  #print(total)
  seats[i] = ifelse(total[i,2] >= total[i,1], 1, 0)
}
#print(seats)
sum(seats)
total

total=matrix(NA,nrow=5,ncol=2)
seats=numeric(Ndist)
for (i in 1:Ndist) {
  # total democrate vote
  total[i,1] = sum(P.data$blue[which(P.data$district == i)])
  # total republican vote
  total[i,2] = sum(P.data$red[which(P.data$district == i)])
  #print(total)
  seats[i] = ifelse(total[i,2] >= total[i,1], 1, 0)
}
#print(seats)
sum(seats)
total


par(mfrow=c(1,1),mar=c(2,2,2,2)+0.1,cex=1.1)
plot(g,asp=0, vertex.color=vcolor[V(g)$district], 
     vertex.frame.color=vcolor[vertex_color],
     main="Proposed Boundary Flips")
legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
                          "District 5","Proposed Vertices"),
       col=vcolor[1:6],pch=c(rep(19,5),1))#,bty="n")

