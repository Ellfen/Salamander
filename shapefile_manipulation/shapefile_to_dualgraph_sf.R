# An R example for working with shape files
library(sf)
library(rgdal)
library(rgeos)
library(shp2graph)
library(deldir)
library(igraph)
library(redist)
# READING WITH RGDAL WHICH HAS BROKEN SEVERAL LINES OF CODE - FIX IN PROGRESS

alabama = st_read("Alabama_shapefiles/tl_2012_01_vtd10.shp")

# Check some parameters
ncol(alabama)
nrow(alabama)
names(alabama)
typeof(alabama)
class(alabama)
str(alabama)

# order by county - not needed but left as placeholder for the code
alabama = alabama[order(alabama$COUNTYFP10),]
# Create subset of first county
Asub = alabama[which(alabama$COUNTYFP10=="001"),]
# plot the map and triangulation side by side
par(mfrow=c(1,2),mar=c(1,1,1,1)+0.1)
plot(Asub$geometry)

centroids = st_centroid(Asub$geometry)
str(centroids)
C = matrix(unlist(centroids), ncol = 2, byrow = TRUE)
C
row.names(C)# = letters[1:dim(C)[1]]
C
points(C[,1],C[,2], pch=NA, text(C[,1],C[,2],labels=labels(C[,1])))
D = deldir(C[,1],C[,2],)
D$delsgs
Elist = cbind(D$delsgs$ind1,D$delsgs$ind2)

g = graph_from_edgelist(Elist,directed = F)
plot(g, layout=layout_with_graphopt(g))
get.edgelist(g)

#How about using redist.adjacency
test = redist.adjacency(Asub)
class(test)
test
# To create an edge list
Elist = numeric(sum(lengths(test))*2)
dim(Elist) = c(sum(lengths(test)),2)
Elist = matrix(NA, nrow=1, ncol=2)
for (i in 1:length(test)) {
  unlist = test[[i]]+1
  unlist = unlist[which(unlist>i)]
  M = cbind(rep(i,length(unlist)), unlist)
  Elist = rbind(Elist,M)
}
Elist=Elist[-1,]
Elist
# How to clean edge list of duplicated data

# To create an adjacency matrix
adjm = matrix(0, nrow=length(test),ncol=length(test))
for (i in 1:length(test)) {
  unlist = test[[i]]
  for (j in 1:length(unlist)) {
    adjm[i,unlist[j]] = 1
  }
}

g = graph_from_edgelist(Elist,directed = F)
plot(Asub$geometry)
points(C[,1],C[,2], pch=NA, text(C[,1],C[,2],labels=labels(C[,1])))
plot(g, layout=layout_with_graphopt(g))
# Create an adjacency matrix to avoid duplicates

# order by county - not needed but left as placeholder for the code
alabama = alabama[order(alabama$COUNTYFP10),]
# Create subset of first county
Asub = alabama[which(alabama$COUNTYFP10=="001"),]
Asub@data
# plot the map and triangulation side by side
par(mfrow=c(1,2),mar=c(1,1,1,1)+0.1)
plot(Asub)
centroid = gCentroid(Asub, byid=T)
points(centroid,pch=19,col=1:length(centroid))
DT = gDelaunayTriangulation(centroid, onlyEdges=T)
plot(DT)
points(centroid,pch=19,col=1:length(centroid))
linesobject=DT@lines
linesobject=linesobject[[1]]
lines = linesobject@Lines
a=linesobject@Lines[[1]]
a@coords
linematrix <- matrix(unlist(lines), ncol = 2, byrow = TRUE)
linematrix

# plot county 001
plot(alabama$geometry[which(alabama$COUNTYFP10 == "001")])
# My subset will be the first county
Asub = alabama[which(alabama$COUNTYFP10=="001"),]
plot(Asub$geometry)
Asubgeo = Asub$geometry
class(Asubgeo)
typeof(Asubgeo)
# can i get as point cloud?
pointcloud = as.matrix(Asubgeo[[1]])
plot(pointcloud,pch=19)
# Can I get the centroid of the shapes
gCentroid(Asubgeo)
gDelaunayTriangulation(Asub)
