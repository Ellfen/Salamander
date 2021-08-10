library(sf)
load("NCDataSub.RData")
st_write(NCDataSub,"NCDataSub.shp",append=F)
