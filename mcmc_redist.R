library(sf)
library(redist)
nodes = 1060
NCshp = st_read("data_cleaning/NCDataSub.shp")
P.data = as.data.frame(NCshp)
P.data = P.data[,-dim(P.data)[2]]
adjlist = redist.adjacency(NCshp)
adjm = matrix(0, nrow=length(adjlist),ncol=length(adjlist))
for (i in 1:length(adjlist)) {
  unlist = adjlist[[i]]+1
  unlist = unlist[which(unlist>i)]
  for (j in 1:length(unlist)) {
    adjm[i,unlist[j]] = 1
  }
}

# Absorb discontiguous nodes
P.data$district[which(P.data$name=="197")] = 1
P.data$district[which(P.data$name=="208")] = 1
P.data$district[which(P.data$name=="221")] = 1
P.data$district[which(P.data$name=="218")] = 1


tic()
R = redist.flip.anneal(NCshp,
                       total_pop = as.vector(P.data$population),
                       ndists = 5)#,
                       #init_plan = P.data$district)#,
                       # pop_tol = 0.01,
                       # areasvec = P.data$area,
                       # counties = P.data$county,
                       # constraint = c("population","compact","countysplit"))
toc()
