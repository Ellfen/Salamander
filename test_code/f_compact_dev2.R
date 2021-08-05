# This script implements code to measure the compactness of a district.
# In order to measure compactness need a measure of the boundary length.
# Two functions will be implemented to test compactness - one that works with
# a grid and one that works with the graph created from a real shape file.
# These inputs can be swapped between by setting binary indicator grid to 1
# if using a grid of 0 is using dummy shapefile adjacency matrix.
library(igraph)
source("../network_functions.R")

############################ SETUP THE NETWORK ################################
# Wrap this up in a function as you use it at the start of every program.
# The input will be 0,1,2 for graph_type.
# select grid = 0,1,2,3,4 for real map, sq, hex respectively.
grid = 5
n = sqrt(1060)
Ndist = 5
Ncounty = 100

gplot = f.graph(n,grid,Ndist,Ncounty)
g = gplot
g = induced_subgraph(gplot,which(V(gplot)$district==5|
                                   V(gplot)$district==10|
                                   V(gplot)$district==11|
                                   V(gplot)$district==9|
                                   V(gplot)$district==12)) #f.perimeter(gplot,nodes)
nodes = vcount(g)

V(g)$district[which(V(g)$name=="408")] = 5
V(g)$district[which(V(g)$name=="419")] = 5
V(g)$district[which(V(g)$name=="429")] = 5
V(g)$district[which(V(g)$name=="432")] = 5

#district = c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4) #c(1,1,2,2,1,1,2,2,3,3,4,4,3,3,4,4)
#V(g)$district = district
district = V(g)$district

# Setup the plotting information
V(g)$size = 10
if (n > 20) {
  V(g)$size = 2 # Reduce size of the nodes
  V(g)$label = NA #V(g)$name # Do not print labels
}

if (grid == 0) {
  graph_attr(g,"layout") = layout_with_graphopt(g, charge=0.0001, 
                                                    mass=30,
                                                    spring.length = 0,
                                                    spring.constant = 1)
  # layout_with_fr is another option worth exploring
  # layout does not always give nice results - might to to do several iterations
  # to get a good one. That completely avoids overlaps.
} else if (grid == 4|grid==5) {
  graph_attr(g,"layout") = layout=cbind(V(g)$centroidx,V(g)$centroidy)
} else {
  graph_attr(g,"layout") = layout_on_grid(g, width = n, height = n, 
                                              dim = 2)
}
vcolor = c("red","violetred1","deeppink3","purple","navy","royalblue",
           "deepskyblue","turquoise3","seagreen","olivedrab2","gold",
           "darkorange1","orange")

# # Can I see the layout information
# graph_attr_names(gplot)
# graph_attr(gplot,"layout")
# graph_attr(gplot,"layout")[,1]
# V(g)$centroidx = graph_attr(gplot,"layout")[,1]
# V(g)$centroidy = graph_attr(gplot, "layout")[,2]

graph_attr(g,"margin") = rep(0.01,4)
par(mar=c(0.5,0.5,0.5,0.5)+0.1)
plot(g, vertex.color=vcolor[get.vertex.attribute(g,"district")], 
     vertex.frame.color=vcolor[get.vertex.attribute(g,"district")])

f.is.contigous1(11,g,district)
# The edgelist won't change so create it now
Elist = get.edgelist(g)
class(Elist) = "numeric"
E(g)$p1 = V(g)$district[Elist[,1]]
E(g)$p2 = V(g)$district[Elist[,2]]
# Make a note of which edges are perimeter edges
# Eint = min(which(Elist[,2]==nodes+1))

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

