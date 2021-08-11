library(sf)
load("NCRejig.RData")
load("../../Data/Runs/LR/Rejig_D.RData")
NCRejigH20K = NCRejig
NCRejigH20K$district = REJIG_D$H
st_write(NCRejigH20K,"../../Data/Runs/LR/SHP/NCRejigH20K.shp",append=F)
