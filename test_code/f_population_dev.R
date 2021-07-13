# To develop a function that outputs a 1 if the population of all districts is 
# within a given percentage and a zero if it is not.
library(igraph)
library(tictoc)
source("district_rules_functions.R")
############################ SETUP THE NETWORK ################################
n = 4 # grid size
nodes = n^2
votes = c(rep(1,ceiling(0.4*nodes)),rep(0,floor(0.6*nodes)))
# Assign an initial partition.
Ndist = 4
district0 = rep(Ndist,nodes)
district0 = replace(district0, 1:(Ndist*round(nodes/Ndist,0)),
                    rep(1:Ndist, each=(nodes/Ndist)))
P.data = data.frame(precinct = 1:nodes, 
                    population = rep(1:4, each=4), 
                    votes_blue = votes, votes_red = as.integer(!votes),
                    district = district0)
P.data$population #sample(1:20, nodes, replace=T)
adjm = f.adjm(n)
E.data = f.edges(adjm)
E.data$length = rep(1, sum(adjm))
# I now have two data frames - one containing precinct data and one containing 
# edge connections and edge lengths.
g = graph_from_data_frame(E.data, directed = FALSE, 
                          vertices=P.data)
plot(g, vertex.color=get.vertex.attribute(g,"district"), 
     layout=layout_on_grid(g))
# Now add district attributes to the edges
for (i in 1:sum(adjm)) {
  E(g)$p1[i] = V(g)$district[as.integer(get.edgelist(g)[i,1])]
  E(g)$p2[i] = V(g)$district[as.integer(get.edgelist(g)[i,2])]
}
# The edgelist won't change so create it now
Elist = get.edgelist(g)
class(Elist) = "numeric"

############################# POPULATION ASSESSMENT ############################
# There are different metrics to measure population deviance. For example if
# I want population to be within 10% of each other could make sure each district
# is within +-5% from the ideal. Alternatively, could calculate all district
# populations, then select the largest and smallest and make sure that these are 
# within 10% of one another
# METHOD 1
deviation = 0.4
distID = c(2,3)
district = V(g)$district; district
pop_ideal = sum(vertex_attr(g,"population"))/Ndist; pop_ideal
ubound = pop_ideal+(deviation*pop_ideal/2); ubound
lbound = pop_ideal-(deviation*pop_ideal/2); lbound
# With this method would only need to calculate the population of the district
# losing a node and the district gaining a node.
tic()
pop = sum(vertex_attr(g,"population")[which(district==distID[1])]); pop
check = ifelse(ubound >= pop & pop >= lbound, 1, 0); check
if (check == 1) {
  pop = sum(vertex_attr(g,"population")[which(district==distID[2])])
  check = ifelse(ubound >= pop & pop >= lbound, 1, 0)
}
toc()
check

b = f.is.balanced(distID,g,district,c(lbound,ubound),pop_ideal)
b
