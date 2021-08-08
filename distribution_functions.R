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