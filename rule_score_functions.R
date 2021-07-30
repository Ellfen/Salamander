# This script contains all rule check functions and score functions

f.is.contigous1 = function(distID, G, district) {
  block = induced_subgraph(g,which(district==distID))
  as.integer(is.connected(block))
}

# Checks if a division on G is contigous. Returns 1 if TRUE and 0 if FALSE
f.is.contigous2 = function(distID, G, district) {
  # Choose a starting vertex uniformly from the district vertices
  v0 = sample(which(district==distID),1)
  # generate a vector of neighbors
  v.neighbors = as.vector(neighbors(G,v0))
  # restrict neighbors to vertices in the same district
  # add these to a queue of vertices to search
  queue = c(v0,v.neighbors[which(district[v.neighbors]==distID)])
  # add queued vertices to explored vector to avoid checking them twice
  explored=queue
  # loop until all vertices in district are searched
  while (length(queue) > 0) {
    for (i in 1:length(queue)) {
      # get neighbors
      v.neighbors = as.vector(neighbors(G,queue[1]))
      # restrict neighbors to those in district
      q = v.neighbors[which(district[v.neighbors]==distID)]
      # restrict neighbors to those not explored
      q = setdiff(q,explored)
      # update the queue
      queue = c(queue[-1],q)
      # update explored
      explored=c(explored,q)
    }
  }
  # each district should have a unique sum - check if the sum of vertices in the
  # district matches the sum of vertices explored
  #check1 = as.integer(sum(explored)== 
                        #sum(which(partition==distID)))
  # As a backup also check if the length of explored equals no.of vertices
  #check2 = as.integer(length(explored)==
                        #length(which(partition==distID)))
  # If both checks are true then the district is connected
  #is.contigous = as.integer(check1 & check2)
  #is.contigous 
  #print(explored)
  is.contigous = as.integer(length(explored)==length(which(district==distID)))
}

# How chosen to implement the test of population in this way as (i) it is the
# way the UK rule on population difference is meant to be implemented (ii) it
# is more efficient as for each proposal only need to calculate the population
# of the districts losing and gaining a node.
f.is.balanced = function(distID, G, district, bound, pop_ideal) {
  # With this method would only need to calculate the population of the district
  # losing a node and the district gaining a node.
  pop = sum(vertex_attr(G,"population")[which(district==distID[1])])
  #print(pop)
  is.balanced = ifelse(bound[2] >= pop & pop >= bound[1], 1, 0)
  #print(is.balanced)
  if (is.balanced == 1) {
    pop = sum(vertex_attr(G,"population")[which(district==distID[2])])
    is.balanced = ifelse(bound[2] >= pop & pop >= bound[1], 1, 0)
  }
  #print(pop)
  is.balanced
}

f.popscore = function(G,district,pop_ideal){
  Ndist = length(unique(district))-1
  pop_dist = numeric(Ndist)
  for (i in 1:Ndist) {
    pop_dist[i] = sum(vertex_attr(G,"population")[which(district==i)])
  }
  #cat("Ndist =",Ndist,"pop_ideal =",pop_ideal,"pop_dist = ",pop_dist,sep=" ")
  Jp = sqrt(sum(((pop_dist/pop_ideal)-1)^2))
  Jp
}

# Function to calculate score, Jc, to measure how many counties are split across
# two or more districts.
# Mc is a large constant
f.countyscore = function(G,district,Mc) {
  # initiate the values
  split2 = 0; split3 = 0; W2 = 0; W3 = 0
  # get the number of counties
  Ncounty = length(unique(V(G)$county))-1
  for (i in 1:Ncounty) {
    # table of how many county vertices are contained across districts
    county_districts = table(district[which(V(G)$county==i)])
    # Across how many districts are the counties split
    splits = length(unique(district[which(V(G)$county==i)]))
    # Loop to sum up 2- and 3-way splits and the weighting
    if (splits == 2) {
      split2 = split2+1
      W2 = W2 + sqrt(min(county_districts)/length(which(V(G)$county==i)))
    } else if (splits == 3) {
      split3 = split3+1
      W3 = W3 + sqrt(min(county_districts)/length(which(V(G)$county==i)))
    } else if (splits > 3) {
      split3 = split3+1
      sorted = sort(county_districts,decreasing = T)
      W3 = W3 + sqrt(sum(sorted[3:length(sorted)])/length(which(V(G)$county==i)))
    }
  }
  # The county split score function.
  Jc = split2*W2 + Mc*split3*W3
  Jc
}

# Compactness score
f.compactscore = function(G,district,p1,p2) {
  Ndist = length(unique(district))-1
  boundary = area = numeric(Ndist)
  #print(boundary)
  #print(area)
  for (i in 1:Ndist) {
    conflicts = c(which(temp_p1==i&temp_p2!=i),which(temp_p2==i&temp_p1!=i))
    boundary[i] = sum(E(g)$weight[conflicts])
    area[i] = sum(V(G)$area[which(district==i)])
  }
  #print(boundary)
  #print(area)
  Ji = sum(boundary^2/area)
  Ji
}