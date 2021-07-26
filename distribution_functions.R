f.seat.count = function(G,Nparty,PartyNames,distribution_party) {
  total=numeric(Nparty)
  seats=character(Ndist)
  for (i in 1:Ndist) {
    for (j in 1:Nparty) {
      total[j] = sum(vertex_attr(g,PartyNames[j])[which(V(g)$district == i)])
    }
    #print(total)
    winner = which(total==max(total))
    seats[i] = ifelse(length(winner) > 1, "Draw", PartyNames[winner])
  }
  #print(seats)
  length(which(seats==distribution_party))
}