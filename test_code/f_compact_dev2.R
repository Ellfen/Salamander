# This script implements code to measure the compactness of a district.
# In order to measure compactness need a measure of the boundary length.
# Two functions will be implemented to test compactness - one that works with
# a grid and one that works with the graph created from a real shape file.
# These inputs can be swapped between by setting binary indicator grid to 1
# if using a grid of 0 is using dummy shapefile adjacency matrix.
library(igraph)
source("../network_functions.R")

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

vcolor = c("turquoise3","gold","olivedrab3","royalblue","darkorange","grey","red")
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
district=V(g)$district
roeck=numeric(Ndist)
for (i in 1:Ndist) {
  xy = cbind(V(g)$centroidx[which(district==i)],
             V(g)$centroidy[which(district==i)])
  
  r = getMinCircle(xy)$rad
  Acircle = r^2*pi #(max(dx,dy)/2)^2*pi
  Adistrict = sum(V(g)$area[which(district==i)])
  roeck[i] = Acircle/Adistrict
}
roeck
sum(roeck)





f.distance = function(x1,x2,y1,y2) {
  lat1 = y1/(180/pi)
  lat2 = y2/(180/pi)
  long1 = x1/(180/pi)
  long2 = x2/(180/pi)
  d_mi = 3963*acos((sin(lat1)*sin(lat2))+cos(lat1)*cos(lat2)*cos(long2-long1))
  d_km = 1.609344*d_mi
  d_km
}
y1 = 53.32055555555556
y2 = 53.31861111111111
x1 = -1.7297222222222221
x2 = -1.6997222222222223
f.distance(x1,x2,y1,y2) # should be 2.00

# For each district, find the minimum/maximum x,y values
#library(sf)
#NCshp = st_read("/Users/laura/Documents/MATH5871_Dissertation/Programming/Rcode/data_cleaning/NCDataGeom.shp")
#plot(NCshp$geometry)

# Get the centroid
# diameter(g)
# farthest_vertices(g)
# plot(g,vertex.color=ifelse(V(g)$name=="557"|V(g)$names=="766","red","blue"))
# vertex_attr_names(g)
# which(V(g)$name=="557")
# V(g)$name[23]
# V(g)$label[23]

compactscore1=compactscore2=roeck=numeric(Ndist)
for (i in 1:Ndist) {
  xy = cbind(V(g)$centroidx[which(district==i)],
                  V(g)$centroidy[which(district==i)])
  minmax_x = rbind(xy[which(xy[,1]==min(xy[,1])),],
                  xy[which(xy[,1]==max(xy[,1])),])
  minmax_x
  #points(minmax_x,pch=19,col="red")
  dx = f.distance(minmax_x[1,1],minmax_x[2,1],minmax_x[2,2],minmax_x[2,2])
  dx3 = f.distance(minmax_x[1,1],minmax_x[2,1],minmax_x[1,2],minmax_x[2,2])

  minmax_y = rbind(xy[which(xy[,2]==min(xy[,2])),],
                  xy[which(xy[,2]==max(xy[,2])),])
  minmax_y
  #points(minmax_y,pch=19,col="red")
  dy = f.distance(minmax_y[1,1],minmax_y[1,1],minmax_y[1,2],minmax_y[2,2])
  dy3 = f.distance(minmax_y[1,1],minmax_y[2,1],minmax_y[1,2],minmax_y[2,2])
  
  Acircle = (max(dx3,dy3)/2)^2*pi
  Aellipse = pi*dx*dy/4
  Pellipse = pi*( (3*(dx+dy)) - sqrt(((3*dx)+dy)*(dx+(3*dy))) )
  Adistrict = sum(V(g)$area[which(district==i)])
  compactscore1[i] = Aellipse/Adistrict
  compactscore2[i] = Pellipse^2/Adistrict
  roeck[i] = Acircle/Adistrict
}
# These are not very good compactness measures
compactscore1
compactscore2
roeck
sum(roeck)
# The graph has been updated with a note of vertices that form the boundary. To
# get district boundaries I want to make a note of all conflicting edges and
# all boundary edges.
# The below code returns the a vector of boundary lengths for each district
# Also need district areas
f.roeck = function(G,district) {
  Ndist = length(unique(district))#-1
  roeck = numeric(Ndist)
  for (i in 1:Ndist) {
    xy = cbind(V(g)$centroidx[which(district==i)],
               V(g)$centroidy[which(district==i)])
    minmax_x = rbind(xy[which(xy[,1]==min(xy[,1])),],
                     xy[which(xy[,1]==max(xy[,1])),])
    dx = f.distance(minmax_x[1,1],minmax_x[2,1],minmax_x[1,2],minmax_x[2,2])
    
    minmax_y = rbind(xy[which(xy[,2]==min(xy[,2])),],
                     xy[which(xy[,2]==max(xy[,2])),])
    dy = f.distance(minmax_y[1,1],minmax_y[2,1],minmax_y[1,2],minmax_y[2,2])
    
    Acircle = (max(dx,dy)/2)^2*pi
    Adistrict = sum(V(g)$area[which(district==i)])
    roeck[i] = Acircle/Adistrict
  }
  Ji = sum(roeck)
  Ji
}
Ji = f.roeck(g,V(g)$district)

