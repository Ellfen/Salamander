# MCMC illustrative run through

# pop check
pideal = 5.33
pop1 = 5
pop2 = 6
pop3 = 5
sqrt( ((pop1/pideal)-1)^2 + ((pop2/pideal)-1)^2 + ((pop3/pideal)-1)^2 ) 

# county split check
split2 = 3
split3 = 0
w2 = sqrt(1/4) + sqrt(2/4) + sqrt(1/4)
w3 = 0
w2*split2 + w3*split3

# Roeck
r1 = 3.122
r2 = 3.122
r3 = 3.122
((pi*r1^2)/(pop1*2.6))+((pi*r2^2)/(pop2*2.6))+((pi*r3^2)/(pop3*2.6))

# Probability distribution
exp(-(0.153+6.67+2.41))
exp(-(0.153+6.49+5.12))

exp(-(0.153+6.49+5.12))/exp(-(0.153+6.67+2.41))
7.79e-6/9.78e-5

Jinit = 0.153+6.67+2.41
J1 = 0.153+6.49+5.12
J2 = 0.153+5.94+2
J3 = 0.405+6.07+4.5
J1-Jinit
J2-Jinit
J3-J2
exp(-2.53)
exp(1.14)
exp(-2.882)
