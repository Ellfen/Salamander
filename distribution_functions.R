# Code for more than two parties
# f.seat.count = function(G,Nparty,PartyNames,distribution_party) {
#   total=numeric(Nparty)
#   seats=character(Ndist)
#   for (i in 1:Ndist) {
#     for (j in 1:Nparty) {
#       total[j] = sum(vertex_attr(g,PartyNames[j])[which(V(g)$district == i)])
#     }
#     #print(total)
#     winner = which(total==max(total))
#     seats[i] = ifelse(length(winner) > 1, "Draw", PartyNames[winner])
#   }
#   #print(seats)
#   length(which(seats==distribution_party))
# }

f.seat.red = function(G,Ndist) {
  total=numeric(2)
  seats=numeric(Ndist)
  for (i in 1:Ndist) {
    # total democrate vote
    total[1] = sum(vertex_attr(G,"blue")[which(V(G)$district == i)])
    # total republican vote
    total[2] = sum(vertex_attr(G,"red")[which(V(G)$district == i)])
    #print(total)
    seats[i] = ifelse(total[2] >= total[1], 1, 0)
  }
  #print(seats)
  sum(seats)
}

f.seat.eff = function(G,Ndist) {
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
  # When the egap is positive - advantage to republicans - otherwise advantage
  # to the democrates
  N = (sum(V(G)$blue)+sum(V(G)$red))
  egap = (colSums(wasted)[1] - colSums(wasted)[2])/N
  #prop = sum(V(G)$red)/N
  out = list("seats"=seats,"egap"=egap)#, "prop"=prop)
  return(out)
}