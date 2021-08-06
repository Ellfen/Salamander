# Import Library for reading shapefiles
library(sf)
library(SpatialEpi)
# Load the shapefile for NC used in 2012 Elections
NC2012shp = st_read("/Users/laura/Documents/MATH5871_Dissertation/Programming/Data/US/NC/tl_2012_37_vtd10/tl_2012_37_vtd10.shp")
# The 2011 shapefile is for districts only (ie. dimensions 13x2)
#NC2011shp = st_read("/Users/laura/Documents/MATH5871_Dissertation/Programming/Data/US/NC/shp/Rucho_Lewis_Congress_3.shp")
names(NC2012shp)
# Do not need state, name, namelsad, lsad, mtfcc, funcstat
# Remove these unnecessary variables
NC2012shp = NC2012shp[,-c(1,6:10)]
# Combine aland and awater to get total area
NC2012shp$AREA = NC2012shp$ALAND10 + NC2012shp$AWATER10
names(NC2012shp)
# For combining datasets we are interested in using GEOID10
NC2012shp = NC2012shp[order(NC2012shp$GEOID10),]

# NC2011block will give the 2011 redistricting information but for blocks
NC2011block = read.csv("/Users/laura/Documents/MATH5871_Dissertation/Programming/Data/US/NC/C-ST-1A.csv",header=T)
names(NC2011block)
NC2011block$Block[1]
# Convert block IDs from scientific notation
NC2011block$Block = format(NC2011block$Block,scientific = F)
head(NC2011block$Block)

# block_VTD gives a means of converting block IDs to VTD keys
block_VTD = read.table("/Users/laura/Documents/MATH5871_Dissertation/Programming/Data/US/NC/Block_Level_Keys.tab",header=T,sep="\t")
names(block_VTD)
# Block_key is the same as block
NC2011block$Block[1:10]
block_VTD$Block_Key[1:10]
# Convert Block_key from scientific
block_VTD$Block_Key = format(block_VTD$Block_Key, scientific = F)
head(block_VTD$Block_Key)
# Order NC2011block and block_VTD according to block code
NC2011block = NC2011block[order(NC2011block$Block),]
block_VTD = block_VTD[order(block_VTD$Block_Key),]
length(unique(NC2011block$Block))
length(unique(block_VTD$Block_Key))
setdiff(unique(NC2011block$Block),unique(block_VTD$Block_Key))
sum(NC2011block$Block==block_VTD$Block_Key)
# These now line up - add VTD keys to NC2011block
NC2011block$VTDKey = block_VTD$VTD_Key
head(NC2011block$VTDKey)
head(NC2012shp$GEOID10)
# Check that all VTD Keys in NC2011block are also in NC2012shp
setdiff(unique(NC2011block$VTDKey),unique(NC2012shp$GEOID10))
# Any NA values for district?
unique(NC2011block$District)
# Find split VTDs
VTDKey = unique(NC2011block$VTDKey)
split = numeric(length(VTDKey))
for (i in 1:length(VTDKey)) {
  VTD_district = NC2011block$District[which(NC2011block$VTDKey==VTDKey[i])]
  districtID = rep(VTD_district[1],length(VTD_district))
  split[i] = ifelse(sum(VTD_district==districtID)==length(VTD_district),0,1)
}
sum(split) #68 VTDs appear to have been split
# Check VTDKey[7]
NC2011block$District[which(NC2011block$VTDKey==VTDKey[7])]
# Part of this VTD is in district 4 and part of it is in district 6.

# Remove all replicates of VTDKey
duplicated_index = which(duplicated(NC2011block$VTDKey)==T)
NC2011block = NC2011block[-duplicated_index,]
# Order this by VTD 
NC2011block = NC2011block[order(NC2011block$VTDKey),]
# Check that this now matches the order of the shapefile
sum(NC2011block$VTDKey==NC2012shp$GEOID10)
# Any NA values for district?
unique(NC2011block$District)


# This has population information
NC2010pop = read.table("/Users/laura/Documents/MATH5871_Dissertation/Programming/Data/US/NC/R2011_VTD.tab",header = T)
names(NC2010pop)[1:10]
NC2010pop[1:10,1:3]
# Comparing this information:
# NC2012$VTDST10 == NC2012$NAME10 ==== VTDdata$VTD_Code == VTDdata$VTD_Name 
# NC2012$GEOID10 ===== VTDdata$VTD_Key
# Keep only the population information - will use total population but keep all
# Could use different population information to make comparisons.
NC2010pop = NC2010pop[,-c(5:19,21:35,37:159)]
names(NC2010pop)
head(NC2010pop)
length(unique(NC2010pop$VTD_Code))
str(NC2010pop)

# This gets me the county codes - dataset is for entire USA
House2016 = read.csv("/Users/laura/Documents/MATH5871_Dissertation/Programming/Data/US/NC/2016-precinct-house.csv",header=T)
# Extract NC
House2016 = House2016[which(House2016$state_postal == "NC"),]
names(House2016)
# Remove unnecessary data
House2016 = House2016[,-c(1,2,3,4,5,15,17,19,23,24,26:37)]
names(House2016)
counties = House2016[,c("county_name","county_fips")]
county_name_fips = unique(counties)
rm(House2016)
rm(counties)
head(county_name_fips)
county_name_fips$county_name = toupper(county_name_fips$county_name)
head(county_name_fips)
county_name_fips$county_name = sub(" .*","",county_name_fips$county_name)
head(county_name_fips)
# There are 100 counties in North Carolina so this is the codes for all of them.

# This gets me the election results
NC2012House = read.delim("/Users/laura/Documents/MATH5871_Dissertation/Programming/Data/US/NC/results_sort_20121106.txt", header=T,sep=",")
NC2012House = NC2012House[order(NC2012House$county,NC2012House$vtd),]
unique(NC2012House$contest)
#grep("US HOUSE OF REPRESENTATIVES",what$contest)
NC2012House = NC2012House[grep("US HOUSE OF REPRESENTATIVES",NC2012House$contest), ]
length(unique(NC2012House$contest))
names(NC2012House)
NC2012House = NC2012House[,-c(3:5,8,10:13,15)]
names(NC2012House)
head(NC2012House)
NC2012House$countycode = rep(NA,dim(NC2012House)[1])
NC2012House$countycode
names(NC2012House)
# I want to create a column that takes the number after US HOUSE OF REP
head(NC2012House$contest)
NC2012House$District_ID = sub(".*DISTRICT ","",NC2012House$contest)
head(NC2012House$District_ID)
class(NC2012House$District_ID) = "numeric"
head(NC2012House$District_ID)

# County is currently given by a name, I want to give county as a code.
# There are 100 counties
for (i in 1:100) {
  index = grep(county_name_fips[i,1],NC2012House$county)
  NC2012House$countycode[index] = county_name_fips[i,2]
}
NC2012House$countycode

# I now want to combine the county code and the VTD to get the vtd_code
length(unique(NC2012House$vtd))
invalid_codes = setdiff(unique(NC2012House$vtd),unique(NC2010pop$VTD_Code))
invalid_codes = invalid_codes[1:6]
for(i in 1:length(invalid_codes)){
  NC2012House = NC2012House[-which(NC2012House$vtd==invalid_codes[i]),]
  
}
setdiff(unique(NC2012House$vtd),unique(NC2010pop$VTD_Code))
# Remove house rows that have liberal candidates
NC2012House = NC2012House[-which(NC2012House$party=="LIB"),]
NC2012House = NC2012House[-which(NC2012House$party==""),]

unique(NC2012House$party)
length(which(NC2012House$party=="DEM"))
length(which(NC2012House$party=="REP"))
length(unique(NC2012House$contest))

NC2012House$VTD_Key = paste(NC2012House$countycode,NC2012House$vtd,sep="")
length(unique(NC2012House$VTD_Key))

# Create the NC Dataframe
unique(NC2011block$District)
NCData = data.frame(unitID=rep(NA,dim(NC2012House)[1]/2))
j = 1
for (i in 1:dim(NCData)[1]) {
  NCData$unitID[i] = NC2012House$VTD_Key[j]
  NCData$county[i] = NC2012House$countycode[j]
  #NCData$district[i] = NC2011block$District[j]
  NCData$blue[i] = NC2012House$total.votes[j]
  j = j+1
  NCData$red[i] = NC2012House$total.votes[j]
  j = j+1
}
sort(unique(NCData$district))

duplicated_index = which(duplicated(NCData$unitID)==T)
NCData=NCData[-duplicated_index,]

diff1 = setdiff(NC2010pop$VTD_Key,NCData$unitID)
diff2 = setdiff(NCData$unitID,NC2010pop$VTD_Key)
which(NC2010pop$VTD_Key==diff1[1])
which(NCData$unitID==diff2[1])

for (i in 1:13) {
  NCData$unitID[which(NCData$unitID==diff2[i])] = diff1[i]
}
diff1 = setdiff(NC2010pop$VTD_Key,NCData$unitID)
diff2 = setdiff(NCData$unitID,NC2010pop$VTD_Key)
diff1
diff2
for (i in 1:9) {
  NCData$unitID[which(NCData$unitID==diff2[i])] = sub("3718500","37185",diff2[i])
}
diff1 = setdiff(NC2010pop$VTD_Key,NCData$unitID)
diff2 = setdiff(NCData$unitID,NC2010pop$VTD_Key)
diff1
diff2
for (i in 1:5) {
  NCData$unitID[which(NCData$unitID==diff2[i])] = sub("371850","37185",diff2[i])
}
diff1 = setdiff(NC2010pop$VTD_Key,NCData$unitID)
diff2 = setdiff(NCData$unitID,NC2010pop$VTD_Key)
diff1
diff2

# Check that all datasets to be combined are in the same order
sum(NCData$unitID==NC2010pop$VTD_Key)
NCData=NCData[order(NCData$unitID),]
NC2010pop=NC2010pop[order(NC2010pop$VTD_Key),]
sum(NCData$unitID==NC2010pop$VTD_Key)
sum(NCData$unitID==NC2011block$VTDKey)

# Add population and district information
NCData$population = NC2010pop$PL10AA_TOT
#NCData$population = NC2010pop$PL10VA_TOT
#NCData$population = NC2010pop$REG10G_TOT
NCData$district = NC2011block$District

# check if populations are balanced - which populations are balanced
pop1 = NC2010pop$PL10AA_TOT
pop2 = NC2010pop$PL10VA_TOT
pop3 = NC2010pop$REG10G_TOT
# population constants
pop_ideal = sum(pop1)/13 # pop_ideal = 733499 for total population
# Based on NC legislature report this should be 733499 which it is for pop1 
# therefore conclude that NC uses total population information not registered 
# voters.
deviation = 0.06 
ubound = pop_ideal+(deviation*pop_ideal/2)
lbound = pop_ideal-(deviation*pop_ideal/2)
pop_bound = c(lbound,ubound); pop_bound #[711494, 755504] for total population
balance_check = numeric(13)
pop = numeric(13)
for (i in 1:13) {
  pop[i] = sum(NCData$population[which(NCData$district==i)])
  balance_check[i] = ifelse(pop_bound[2] >= pop[i] & pop[i] >= pop_bound[1], 1, 0)
}
balance_check
pop
sum(pop)
sum(pop1) #sum(pop1) = 9535483
sort(unique(NCData$district))
length(NCData$district)
unique(NCData$district)

# My district information might be wrong


# Want to add the area information
setdiff(NCData$unitID, NC2012shp$GEOID10)
sum(NCData$unitID==NC2012shp$GEOID10)
# These codes are the same and they are already in the same order
NCData$area = NC2012shp$AREA
# To save without geometry
save(NCData,file="NCData.RData")

# To add geometry to existing file - can just run from here and run very
# beginning that loads NC2012shp
load("/Users/laura/Documents/MATH5871_Dissertation/Programming/Rcode/data_cleaning/NCData.RData")
NCDataGeom = NCData
setdiff(NCDataGeom$unitID,NC2012shp$GEOID10)
sum(NCDataGeom$unitID==NC2012shp$GEOID10)
# UnitIDs are equal and in the same order.

# Investigate area
sum(NCDataGeom$area)
# appears to be in metres^2 - divide by 1000^2 to get in km sq
sum(round(NCDataGeom$area/(1000^2),2))
NCDataGeom$area = round(NCDataGeom$area/(1000^2),2)
sum(NCDataGeom$area)
NCDataGeom$area[1]

# To test centroids with county 37001
# plot(NC2012shp$geometry[which(NCDataGeom$county==37001)])
# centroids = st_centroid(NC2012shp$geometry[which(NCDataGeom$county==37001)])
# plot(centroids, add=T,pch=19)
# str(centroids)
# centroids = matrix(unlist(centroids), ncol = 2, byrow = TRUE)
# centroidx = centroids[,1]
# centroidy = centroids[,2]
# points(centroidx,centroidy, pch=19, col="red")
# Not sure what points intplat intplon give?
#points(NC2012shp$INTPTLAT10[1:20],NC2012shp$INTPTLON10[1:20])

# Centroids for the entire data frame
centroids = st_centroid(NC2012shp$geometry)
centroids = matrix(unlist(centroids), ncol = 2, byrow = TRUE)
# How does the centroid relate to the long lat
centroids[1:10,]
NC2012shp$INTPTLAT10[1:10]
NC2012shp$INTPTLON10[1:10]
# it goes long lat
centroids = latlong2grid(centroids)
head(centroids)

centroidx = centroids[,1]
centroidy = centroids[,2]
# Add to the data frame with geometry info
NCDataGeom$centroidx = centroidx
NCDataGeom$centroidy = centroidy
NCDataGeom$geometry = NC2012shp$geometry


names(NCDataGeom)
head(NCDataGeom)
# Before saving add a name column
NCDataGeom$name = 1:dim(NCDataGeom)[1]
NCDataGeom = NCDataGeom[,c(dim(NCDataGeom)[2],1:(dim(NCDataGeom)[2]-1))]
head(NCDataGeom)

# To save complete NC shapefile
save(NCDataGeom,file="NCDataGeom.RData")
st_write(NCDataGeom,"NCDataGeom.shp", append=F)

# Subset of districts 5,9,10,11,12
NCDataSub = NCDataGeom
unique(NCDataSub$district)
index = which(NCDataSub$district!=5&
                NCDataSub$district!=9&
                NCDataSub$district!=10&
                NCDataSub$district!=11&
                NCDataSub$district!=12)
NCDataSub = NCDataSub[-index,]
unique(NCDataSub$district)
NCDataSub$name = 1:dim(NCDataSub)[1]
# Re-label the districts
NCDataSub$district[which(NCDataSub$district==5)] = 1
NCDataSub$district[which(NCDataSub$district==9)] = 4
NCDataSub$district[which(NCDataSub$district==10)] = 3
NCDataSub$district[which(NCDataSub$district==11)] = 2
NCDataSub$district[which(NCDataSub$district==12)] = 5

unique(NCDataSub$district)

save(NCDataSub,file="NCDataSub.RData")
st_write(NCDataSub,"NCDataSub.shp", append=F)
