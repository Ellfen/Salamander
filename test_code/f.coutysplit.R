library(igraph)
library(tictoc)
#library(Rfast)
source("../grid_functions.R")
source("../network_functions.R")
#source("district_rules_functions.R")
#source("distribution_functions.R")
n = 6
Ndist = 4
Ncounty = 3
g = f.graph(n,2,Ndist,Ncounty,"../")
vertex_attr_names(g)
V(g)$district
V(g)$county

# Setup the plotting information
V(g)$size = 10
V(g)$label = V(g)$county 
graph_attr(g,"layout") = layout_on_grid(g, width = n, height = n, dim = 2)
graph_attr(g,"margin") = rep(0.1,4)
par(mar=c(0.5,0.5,0.5,0.5)+0.1)
plot(g, vertex.color=get.vertex.attribute(g,"district"), 
     vertex.frame.color=get.vertex.attribute(g,"district"),
     edge.color=get.edge.attribute(g,"p1"))

test = table(c(1,1,1,2,2,2,2,3,3,4,5,6,7))
test
sort(test,decreasing = T)
sum(test[3:length(test)])

# Want to find out how many counties are split between 2 districts
f.countysplit = function(G,Mc) {
  # initiate the values
  split2 = 0; split3 = 0; W2 = 0; W3 = 0
  Ncounty = length(unique(V(g)$county))
  for (i in 1:Ncounty) {
    county_districts = table(V(g)$district[which(V(g)$county==i)])
    splits = length(unique(V(g)$district[which(V(g)$county==i)]))
    if (splits == 2) {
      split2 = split2+1
      W2 = W2 + sqrt(min(county_districts)/length(which(V(g)$county==i)))
    } else if (splits == 3) {
      split3 = split3+1
      W3 = W3 + sqrt(min(county_districts)/length(which(V(g)$county==i)))
    } else if (splits > 3) {
      split3 = split3+1
      sorted = sort(county_districts,decreasing = T)
      W3 = W3 + sqrt(sum(sorted[3:length(sorted)])/length(which(V(g)$county==i)))
    }
  }
  Jc = split2*W2 + Mc*split3*W3
  Jc
}
f.countysplit(g,1000)
