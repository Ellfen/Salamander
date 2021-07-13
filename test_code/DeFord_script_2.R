# An updated implementation of using drawing a scrabble tile from the bag
# as the proposal distribution and the score on scrabble tiles as the score 
# function.

# A vector of the score associated with each scrabble tile and the probability 
# plot
scrabble_scores = c(1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10,1)
p_scores = scrabble_scores/sum(scrabble_scores)
barplot(p_scores, ylim=c(0,0.12), names.arg=c(letters,""))
# A vector of how many of each tile are in a set and probability plot
scrabble_freq = c(9,2,2,4,12,2,3,2,9,1,1,4,2,6,8,2,1,6,4,6,4,2,2,1,2,1,2)
p_counts = scrabble_freq/sum(scrabble_freq); p_counts
barplot(p_counts, ylim=c(0,0.12), names.arg=c(letters,""))

# Now lets implement Metropolis-Hastings algorithm in simplified form
# We need a proposal function and score function.
# Our proposal function is the act of drawing a tile from a scrabble bag. We 
# can sample from the numbers 1-27 with probability given by p_counts.
# Our state space is the collection of tiles in the scrabble bag therefore 
# our initial state X0 should be a value 1-27
# Our transition probability is... is it uniform or p_counts? I think it is 
# p_counts because given I am at state x the probability of being at y does not
# change.
# Our score function is the scrabble scores

# M-H algorithm
# Pick a starting value X0
Xj = sample(1:27, 1, prob=p_counts)
# Define how many iterations
N = 10000
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
# Plot the outcome
plot(1:50, X[1:50], "b")
table(X)
p_out = table(X)/N; p_out
sum(p_out)
typeof(p_out)
p_out = as.matrix(p_out)
x_values = labels(p_out)[[1]]; x_values
typeof(x_values)
x_values = as.numeric(x_values); x_values

bp = barplot(p_scores, ylim=c(0,0.12), names.arg=c(letters,""))
points(bp[x_values], p_out, pch=16, col="red")
