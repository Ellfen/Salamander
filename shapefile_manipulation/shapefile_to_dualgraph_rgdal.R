# An R example for working with shape files
library(sf)
library(rgdal)
library(rgeos)
library(shp2graph)
# READING WITH RGDAL WHICH HAS BROKEN SEVERAL LINES OF CODE - FIX IN PROGRESS
sf = 1
if (sf == 0) {
  alabama = readOGR(dsn="Alabama_shapefiles", layer="tl_2012_01_vtd10")
} else {
  alabama = st_read("Alabama_shapefiles/tl_2012_01_vtd10.shp")
}
# Check some parameters
ncol(alabama)
nrow(alabama)
names(alabama)
typeof(alabama)
class(alabama)
# Using readOGR you get a spatialpolygonsdataframe. Using st_read you get df.
alabama = as(alabama, "SpatialLinesDataFrame")
class(alabama)
names(alabama)
# order by county - not needed but left as placeholder for the code
alabama = alabama[order(alabama$COUNTYFP10),]
# Create subset of first county
Asub = alabama[which(alabama$COUNTYFP10=="001"),]
# plot the map and triangulation side by side
par(mfrow=c(1,1),mar=c(1,1,1,1)+0.1)
plot(Asub)

# Use shp2graph function to get node and edgelist
test = readshpnw(Asub)
test

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
