# This script contains all rule check functions

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
  pop = sum(vertex_attr(g,"population")[which(district==distID[1])])
  #print(pop)
  is.balanced = ifelse(bound[2] >= pop & pop >= bound[1], 1, 0)
  #print(is.balanced)
  if (is.balanced == 1) {
    pop = sum(vertex_attr(g,"population")[which(district==distID[2])])
    is.balanced = ifelse(bound[2] >= pop & pop >= bound[1], 1, 0)
  }
  #print(pop)
  is.balanced
}