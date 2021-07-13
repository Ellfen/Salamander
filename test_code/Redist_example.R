# Copying the example from alarm-redist.github.io/redist/
library(redist)
library(dplyr)

data(iowa)
str(iowa)
View(iowa)
# The Iowa data file has 16 variables:
# $name is the name of the counties
# $pop is the county population with its breakdown into white, black, hisp
# $tot_08 might be the total number of registered voters with a breakdown
#  of democrates and republicans
# $region gives the geographic region
# $fips... not sure?
# $cd_2010 might be which district a county belongs to according to the 2010
#  map. According to wikipedia iowa has four congressional districts
length(unique(as.vector(iowa$cd_2010)))
# $not sure about vap, wvap, bvap, hvap??
str(iowa$geometry)
typeof(iowa$geometry)
iowa$geometry
# I don't understand how to work with the geometry data.

# Continuing with the example
# Set a 0.01% population constraint
iowa_map = redist_map(iowa, existing_plan=cd_2010, pop_tol = 0.0001, 
                      total_pop = pop)
str(iowa_map)
# Using redist_map adds a column adj
# simulate 250 plans using the SMC algorithm
iowa_plans = redist_smc(iowa_map, nsims=250, verbose=FALSE)

# plot them
library(ggplot2)
library(patchwork) # for plotting

redist.plot.plans(iowa_plans, draws=c("cd_2010", "140", "220", "30"),
                  geom=iowa_map)
