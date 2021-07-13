# An R example for working with shape files
library(sf)
library(rgdal)
library(rgeos)
# READING WITH RGDAL WHICH HAS BROKEN SEVERAL LINES OF CODE - FIX IN PROGRESS
alabama = readOGR(dsn="Alabama_shapefiles", layer="tl_2012_01_vtd10")
ncol(alabama)
nrow(alabama)
names(alabama)
typeof(alabama)
class(alabama)
typeof(alabama@data)
str(alabama@data)
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
