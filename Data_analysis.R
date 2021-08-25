######################## IMPORT FUNCTIONS AND LIBRARIES ########################
library(igraph)
library(tictoc)
source("network_functions.R")
source("rule_score_functions.R")
source("distribution_functions.R")
library(sf)
###############################################################################
# Load the sampling data
load("REAL_500_2_2_S4.RData")
head(REAL_500_2_2_S4)
dim(REAL_500_2_2_S4)

############################### FOR PLOTS #####################################
# Load the map configuration as stood after sampling
load("../Data/Runs/LR/REAL_D.RData")
# Load the original map
NCOriginal = st_read("data_cleaning/NCOriginal.shp")
NCSampled = NCOriginal
NCSampled$district = REAL_D$S

# Create the original graph object
P.data = as.data.frame(NCOriginal)
P.data = P.data[,-dim(P.data)[2]]
adjlist = redist.adjacency(NCOriginal)
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
goriginal = graph_from_data_frame(E.data, directed = FALSE, 
                          vertices=P.data)
# Absorb discontinuities
V(goriginal)$district[which(V(goriginal)$name=="197")] = 1
V(goriginal)$district[which(V(goriginal)$name=="208")] = 1
V(goriginal)$district[which(V(goriginal)$name=="221")] = 1
V(goriginal)$district[which(V(goriginal)$name=="218")] = 1

# Setup the plotting information
V(goriginal)$size = 2 # Reduce size of the nodes
V(goriginal)$label = NA # Do not print labels
graph_attr(goriginal,"layout") = cbind(V(goriginal)$centroidx,
                                       V(goriginal)$centroidy)
vcolor = c("turquoise3","gold","olivedrab3","royalblue","darkorange","black",
           "red")
graph_attr(goriginal,"margin") = rep(0.01,4)

# Create the sampled graph
gsampled = goriginal
V(gsampled)$district = REAL_D$S

#plot the maps side by side
# CHANGE THE PLOT TITLE
par(mfrow=c(1,2),mar=c(2,2,2,0.5)+0.1)
plot(goriginal, asp=0, vertex.color=vcolor[V(goriginal)$district], 
     vertex.frame.color=vcolor[V(goriginal)$district], 
     main="")
# legend("topleft",legend=c("District 1","District 2", "District 3","District 4",
#                           "District 5"),
#        col=vcolor[1:5],pch=19)#,bty="n")
plot(gsampled, asp=0, vertex.color=vcolor[V(gsampled)$district], 
     vertex.frame.color=vcolor[V(gsampled)$district], 
     main="")
# CHANGE THE FILE NAME
#dev.copy2pdf(file="../Images/name.pdf")

# Get metrics for each map - note that the sampled map is not necessarily a 
# accepted map - may need to run though several iterations of the algorithm to get
# one that meets admissible requirements.

################################# CLEANING ####################################
data = REAL_500_2_2_S4[20000:140077,]
data$admissible = as.integer(data$balanced & data$tied)
data = data[-which(data$accepted==0),]
data = data[which(data$admissible==1),]
# thinning value
n = 10
index = seq(1,5051,n)
data = data[index,]
# N value for each of the estimates for error epsilon
epsilon1 = 0.1
epsilon2 = 0.001
N_egap = var(data$effgap)/(epsilon2^2)
N_roeck = var(data$Ji)/(epsilon1^2)
# Calculate the autocorrelation
rho_egap = acf(data$effgap)
rho_roeck = acf(data$Ji)
# Calculate how much you need to inflate N by
inflation_egap = (1+(2*sum(rho_egap$acf)))
inflation_roeck = (1+(2*sum(rho_roeck$acf)))
# what N is required
N_egap*inflation_egap
N_roeck*inflation_roeck
# lets look at acf over full sample
acf(data$effgap,lag.max = 500)
acf(data$Ji, lag.max=500)

# Do the calculations the opposite way - what error will I get with a sample
# size of 1000
# N/Neff = inflation
# My inflated sample is approx 1000
M_egap = 1000/inflation_egap
M_roeck = 1000/inflation_roeck

MSE_egap = sqrt(var(data$effgap)/M_egap)
MSE_roeck = sqrt(var(data$Ji)/M_roeck)
