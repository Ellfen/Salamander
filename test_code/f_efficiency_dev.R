######################## IMPORT FUNCTIONS AND LIBRARIES ########################
library(igraph)
library(tictoc)
source("../network_functions.R")
source("../distribution_functions.R")
############################ SETUP THE NETWORK ################################
# select grid = 0,1,2,3,4,5 for real map, sq, hex, NCdummy, NCreal, NCsub
grid = 2
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
     asp=1)

# The edgelist won't change so create it now
Elist = get.edgelist(g)
class(Elist) = "numeric"
E(g)$p1 = V(g)$district[Elist[,1]]
E(g)$p2 = V(g)$district[Elist[,2]]
# Make a note of which edges are perimeter edges
# Eint = min(which(Elist[,2]==nodes+1))

V(g)$blue
V(g)$red
f.seat.red(g,Ndist)

f.seat.red = function(G,Ndist) {
  total=numeric(2)
  seats=numeric(Ndist)
  wasted=matrix(nrow=Ndist,ncol=2)
  for (i in 1:Ndist) {
    # total democrate vote
    total[1] = sum(vertex_attr(G,"blue")[which(V(G)$district == i)])
    # total republican vote
    total[2] = sum(vertex_attr(G,"red")[which(V(G)$district == i)])
    #print(total)
    seats[i] = ifelse(total[2] >= total[1], 1, 0)
    if (total[2] >= total[1]){
      # red wins
      wasted[i,1] = total[1]
      wasted[i,2] = total[2] - 0.5*(total[1]+total[2])
    } else {
      # blue wins
      wasted[i,1] = total[1] - 0.5*(total[1]+total[2])
      wasted[i,2] = total[2]
    }
  }
  #print(seats)
  seats = sum(seats)
  egap = (colSums(wasted)[1] - colSums(wasted)[2])/(sum(V(G)$blue)+sum(V(G)$red))
  out = list("seats"=seats,"egap"=egap)
  return(out)
}

f.seat.red(g,Ndist)
