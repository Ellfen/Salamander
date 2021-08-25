# An updated implementation of using drawing a scrabble tile from the bag
# as the proposal distribution and the score on scrabble tiles as the score 
# function.

# A vector of the score associated with each scrabble tile and the probability 
# plot
# A vector of how many of each tile are in a set and probability plot
p_counts = rep(1/5,5); p_counts
par(mfrow=c(1,2),mar=c(3,3,2,2)+0.1)
barplot(p_counts, names.arg=letters[1:5], ylim=c(0,0.2), 
        main="Distribution of Proposals")
scrabble_scores = 1:5
p_scores = scrabble_scores/sum(scrabble_scores)
barplot(p_scores, names.arg=letters[1:5], ylim=c(0,0.35),
        main="Stationary Distribution")

# M-H algorithm
# Pick a starting value X0
Xj = sample(1:5, 1, prob=p_counts)
# Define how many iterations
N = 100
# Define a vector to store the Markov chain
X = numeric(N)
# Define alpha
f.alpha <- function(x, y) {
  min((p_scores[y]*p_counts[x])/(p_scores[x]*p_counts[y]), 1)
}
# Run the loop
for (j in 1:N) {
  Yj = sample(1:27, 1, prob=p_counts)
  Uj = runif(1)
  Xj = ifelse(Uj <= f.alpha(Xj, Yj), Yj, Xj)
  X[j] = Xj
}