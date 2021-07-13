# A script to accompany DeFords pdf MCMC for redistricting

# Calculating the empirical distribution of alphabet path
# The transition matrix is 26x26 with zeroes on the diagonal and 0.5 either 
# side of the diagonal with exception of 1 for "a" to "b" and "z" to "y".
Tij <- matrix(0,26,26); Tij
diag(Tij[-1,])<-c(rep(0.5,24),1); Tij
diag(Tij[,-1])<-c(1,rep(0.5,24)); Tij
# If we start at "a" and run once then i=1 and the probability of being at "b"
# j=2 should be 1 and the prob of being at c, j=3 should be 0:
Q_b = c(1,rep(0,25))%*%Tij[,2]; Q_b
Q_c = c(1,rep(0,25))%*%Tij[,3]; Q_c
# If we start at "b" and run once then i=2 and p(a)=0.5, p(c)=0.5, p(other)=0
Q_a = replace(rep(0, 26),2,1)%*%Tij[,1]; Q_a
Q_c = replace(rep(0, 26),2,1)%*%Tij[,3]; Q_c
Q_other = replace(rep(0,26),2,1)%*%Tij[,5]; Q_other
# To find the distribution of j=1 ie. a 
# How can you find the empirical distribution???
N = 1e6
Q = numeric(N)
state = sample(1:26,1); state
Q[1] = replace(rep(0, 26),state,1)%*%Tij[,1]; Q[1]
for (iter in 1:N) {
  if (state == 1) {state = 2} else 
    if (state == 26) {state = 25} else 
      {state = sample(c(state+1,state-1),1)}
  Q[iter+1] = replace(rep(0, 26),state,1)%*%Tij[,1]
}
mean(Q)
# This appears to work but it is very slow - how could I make it faster?

# Lets now make the alphabet path aperiodic by including a probability for 
# staying in the current state.
Tij <- diag(0.2,26,26); Tij[1:6,1:6]
diag(Tij[-1,])<-c(rep(0.4,24),0.8)
diag(Tij[,-1])<-c(0.8,rep(0.4,24))
Tij[1:6,1:6]
# Now find the empirical
N = 1e6
j = 2
Q = numeric(N)
state = sample(1:26,1); state
Q[1] = replace(rep(0, 26),state,1)%*%Tij[,j]; Q[1]
for (iter in 1:N) {
  if (state == 1) {state = sample(c(state,2),1,prob=c(0.2,0.8))} else 
    if (state == 26) {state = sample(c(state,25),1,prob=c(0.2,0.8))} else 
    {state = sample(c(state+1,state,state-1),1,prob=c(0.4,0.2,0.4))}
  Q[iter+1] = replace(rep(0, 26),state,1)%*%Tij[,j]
}
mean(Q)
# The empircal distribution for "a" and "z" is 0.02
# The empircal distribution for all the rest is 0.04
Pi = c(0.02, rep(0.04,24), 0.02)
sum(Pi)
Pj = Pi%*%Tij; Pj

# What about if I have equal probabilities for staying current state or moving
Tij <- diag(0.34,26,26); Tij[1:6,1:6]
diag(Tij[-1,])<-c(rep(0.33,24),0.66)
diag(Tij[,-1])<-c(0.66,rep(0.33,24))
Tij
rowSums(Tij)
# Now find the empirical
N = 1e6
# Change between j=1 for "a" and "z" and j=2 for the others
j = 2
Q = numeric(N)
state = sample(1:26,1); state
Q[1] = replace(rep(0, 26),state,1)%*%Tij[,j]; Q[1]
for (iter in 1:N) {
  if (state == 1) {state = sample(c(state,2),1,prob=c(0.2,0.8))} else 
    if (state == 26) {state = sample(c(state,25),1,prob=c(0.2,0.8))} else 
    {state = sample(c(state+1,state,state-1),1,prob=c(0.4,0.2,0.4))}
  Q[iter+1] = replace(rep(0, 26),state,1)%*%Tij[,j]
}
mean(Q)
# The empircal distribution for "a" and "z" is still 0.02 is this correct?
# The empircal distribution for all the rest is still 0.04 is this correct?
Pi = c(0.02, rep(0.04,24), 0.02)
sum(Pi)
Pj = Pi%*%Tij; Pj

eigT = eigen(t(Tij))
pi_stat = eigT$vectors[,1]; pi_stat
pi_stat = round(pi_stat/sum(pi_stat),2); pi_stat
sum(pi_stat)

# Calculating the probabilities of scrabble tiles using scores
scrabble_scores = c(1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10,1)
length(scrabble_scores)
denom = sum(scrabble_scores); denom
p_scores = scrabble_scores/denom; p_scores
barplot(p_scores, ylim=c(0,0.12), names.arg=c(letters,""))
# Calculating the probs or drawing scrabble tiles, last value is blank tile
scrabble_freq = c(9,2,2,4,12,2,3,2,9,1,1,4,2,6,8,2,1,6,4,6,4,2,2,1,2,1,2)
p_counts = scrabble_freq/sum(scrabble_freq); p_counts
barplot(p_counts, ylim=c(0,0.12), names.arg=c(letters,""))
sum(p_counts)
Tij = rep(p_counts, 27)
Tij = matrix(Tij, nrow=27, ncol=27, byrow=T); Tij[1:6,1:6]
Q = Tij
y = sample(1:27, 1); y
N = 100000
X = numeric(N)
proposals = numeric(N)
for (i in 1:N) {
  yhat = sample(1:27,1,prob=p_counts)
  proposals[i] = yhat
  alpha = min(1, (scrabble_scores[yhat]*Q[yhat,y])/
                (scrabble_scores[y]*Q[y,yhat]))
  beta = runif(1)
  y = ifelse(beta < alpha, yhat, y)
  X[i] = y
}
par(mfrow=c(2,2))
hist(X, breaks = 27, xlim=c(1,27), prob=T)
barplot(p_scores, ylim=c(0,0.12))
hist(proposals, breaks = 27, xlim=c(1,27), prob=T)
barplot(p_counts, ylim=c(0,0.12))
# I am not accepting any "a" which is odd as I accept other letters with equal
# probability
sort(unique(proposals))
sort(unique(X))
# Something really weird is happening. In the X vector I am getting 100.12 etc.
# That problem appears to have gone now but I am one stick short in my histogram
par(mfrow=c(1,1))
plot(proposals[100:400])
lines(X[100:400])
abline(h=1, col="blue")
abline(h=2, col="blue")
# I don't understand - keep reading.
test = as.data.frame(table(X))
str(test)
X_dist = test$Freq/sum(test$Freq)
par(mfrow=c(1,2))
barplot(X_dist, ylim=c(0,0.12))
barplot(p_scores, ylim=c(0,0.12))
# I think the issue might have been with how I was plotting the results.
# We can measure how successful we were using the Total Variation
sum(abs(p_scores-X_dist))

# Section 5 on mixing times
# Proposal distribution is the alphabet path including a space at the end
proposal = c(0.02, rep(0.04,25), 0.02)
# Target distribution is the alphabet score
target = 1:27/sum(1:27); target
sum(target)
# Implement MCMC based on stat theory method
# Proposal distribution is the alphabet path therefore Q is a matrix with 0 on
# the diagonal, 0.5 off the diagonal with exception of 1 in first and last
# position.
Tmatrix = matrix(0,27,27)
diag(Tmatrix[-1,])<-c(rep(0.5,25),1)
diag(Tmatrix[,-1])<-c(1,rep(0.5,25))
Tmatrix[1:6,1:6]
Tmatrix[23:27,23:27]
# The transition matrix is symmetric unless you are at a or space. Lets try 
# implementing MCMC using the matrix or using if statement, is one faster?
f.alpha1 = function(y, yhat) {
  min(1, (target[yhat]*Tmatrix[yhat,y])/(target[y]*Tmatrix[y, yhat]))
}
# if yhat = 1 it means y = 2, T[1,2] = 1, T[2,1] = 0.5 so 
# T[yhat,y]/T[y,yhat] = 1/0.5 = 2
# if yhat = 27 it means y = 26, T[27,26] = 1, T[26,27] = 0.5 so same as above
f.alpha2 = function(y, yhat) {
  if (yhat == 1 | yhat == 27) min(1, 2*target[yhat]/target[y]) else
    if ((yhat==2 & y==1)|(yhat==26 & y==27)) min(1, 0.5*target[yhat]/target[y]) else
      min(1, target[yhat]/target[y])
}
library(tictoc)
N = 1000
Y = numeric(N)
TV = numeric(N)
y = 1
tic()
for (j in 1:N) {
  if (y == 1) yhat = 2 else
    if (y == 27) yhat = 26 else
      yhat = y + sample(c(-1,1),1,prob=c(0.5,0.5))
  beta <- runif(1)
  y <- ifelse(beta < f.alpha1(y, yhat), yhat, y)
  Y[j] <- y
  Yfreq = as.data.frame(table(Y)); Yfreq
  Ydist = Yfreq$Freq/sum(Yfreq$Freq)
  TV[j] = sum(abs(Ydist - target))
}
toc()
length(TV)
plot(1:N, TV)
abline(h=0.25, col="blue")

N = 1000
Y = numeric(N)
TV = numeric(N)
y = sample(1:27, 1)
tic()
for (j in 1:N) {
  if (y == 1) yhat = 2 else
    if (y == 27) yhat = 26 else
      yhat = y + sample(c(-1,1),1,prob=c(0.5,0.5))
    beta <- runif(1)
    y <- ifelse(beta < f.alpha2(y, yhat), yhat, y)
    Y[j] <- y
}
toc()
# Using if statements in the alpha function is slower.