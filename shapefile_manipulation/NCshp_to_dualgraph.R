library(redist)
library(sf)
source("/Users/laura/Documents/MATH5871_Dissertation/Programming/Rcode/network_functions.R")
# Load the dataframe which has been saved as a shapefile
NCshp = st_read("/Users/laura/Documents/MATH5871_Dissertation/Programming/Rcode/data_cleaning/NCDataGeom.shp")
NCshp[1:10,]
# TO TEST: create a subset of county 37001 and replace all instances of NCshp
# with NCsub
# NCsub = NCshp[which(NCshp$county==37001),]
# par(mar=c(1,1,1,1)+0.1)
# plot(NCsub$geometry)
# points(NCsub$centroidx,NCsub$centroidy,pch=19)

adjlist = redist.adjacency(NCshp)
class(adjlist)
# To create an edge list
# Elist = numeric(sum(lengths(adjlist))*2)
# dim(Elist) = c(sum(lengths(adjlist)),2)
# Elist = matrix(NA, nrow=1, ncol=2)
# for (i in 1:length(adjlist)) {
#   unlist = adjlist[[i]]+1
#   unlist = unlist[which(unlist>i)]
#   M = cbind(rep(i,length(unlist)), unlist)
#   Elist = rbind(Elist,M)
# }
# Elist=Elist[-1,]
# Elist

# To create an adjacency matrix
adjm = matrix(0, nrow=length(adjlist),ncol=length(adjlist))
for (i in 1:length(adjlist)) {
  #unlist = test[[i]]
  unlist = adjlist[[i]]+1
  unlist = unlist[which(unlist>i)]
  for (j in 1:length(unlist)) {
    adjm[i,unlist[j]] = 1
  }
}
E.data = f.edges(adjm)

# From an edgelist but actually want to do from a dataframe!
# g = graph_from_edgelist(Elist,directed = F)
# dev.new()
# plot(NCsub$geometry)
# points(NCsub$centroidx,NCsub$centroidy, pch=NA, 
#        text(NCsub$centroidx,NCsub$centroidy,labels=labels(NCsub$centroidx)))
# plot(g, layout=layout_with_graphopt(g))

# To create from a data frame
names(NCshp)
P.data = as.data.frame(NCshp)
head(P.data)
P.data=P.data[,-11]
head(P.data)
g = graph_from_data_frame(E.data,directed = F,vertices=P.data)
# par(mfrow=c(1,2))
# plot(NCsub$geometry)
# points(NCsub$centroidx,NCsub$centroidy, pch=NA, 
#        text(NCsub$centroidx,NCsub$centroidy,labels=labels(NCsub$centroidx)))
# Setup the plotting information
V(g)$size = 2 # Reduce size of the nodes
V(g)$label = NA # Do not print labels
graph_attr(g,"margin") = rep(0.01,4)
par(mfrow=c(1,1),mar=c(0.5,0.5,0.5,0.5)+0.1)
plot(g, vertex.color=get.vertex.attribute(g,"district"), 
     vertex.frame.color=get.vertex.attribute(g,"district"),
     layout=cbind(NCshp$centroidx,NCshp$centroidy))

# The edgelist I constructed and the graph edge list are the same.
gElist = get.edgelist(g)
class(gElist) = "numeric"
colSums(Elist!=gElist)
