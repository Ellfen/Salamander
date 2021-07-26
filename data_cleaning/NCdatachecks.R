library(sf)
# Load the shapefile for NC
NC2012shp = st_read("../../Data/US/NC/tl_2012_37_vtd10/tl_2012_37_vtd10.shp")
# Do not need state, name, namelsad, lsad, mtfcc, funcstat
NC2012shp = NC2012shp[,-c(1,6:10)]
# Combine aland and awater to get total area
NC2012shp$AREA = NC2012shp$ALAND10 + NC2012shp$AWATER10
names(NC2012shp)
# What does the VTD information look like
NC2012shp = NC2012shp[order(NC2012shp$COUNTYFP10,NC2012shp$VTDST10),]
NC2012shp$VTDST10[1:10]
NC2012shp$VTDI10[1:10]
head(NC2012shp)

# This has population information
NC2010pop = read.table("../../Data/US/NC/R2011_VTD.tab",header = T)
names(NC2010pop)[1:10]
NC2010pop[1:10,1:3]
# Comparing this information:
# NC2012$VTDST10 == NC2012$NAME10 ==== VTDdata$VTD_Code == VTDdata$VTD_Name 
# NC2012$GEOID10 ===== VTDdata$VTD_Key
# Keep only the population information - will use voting age population but keep
# all three totals
NC2010pop = NC2010pop[,-c(5:19,21:35,37:159)]
names(NC2010pop)
head(NC2010pop)
length(unique(NC2010pop$VTD_Code))
str(NC2010pop)

# This gets me the county codes
House2016 = read.csv("../../Data/US/NC/2016-precinct-house.csv",header=T)
#load("2016-precinct-house.RData")
#House2016 = x
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


# This gets me the election results
NC2012House = read.delim("../../Data/US/NC/results_sort_20121106.txt", header=T,sep=",")
NC2012House = NC2012House[order(NC2012House$county,NC2012House$vtd),]
unique(NC2012House$contest)
#grep("US HOUSE OF REPRESENTATIVES",what$contest)
NC2012House = NC2012House[grep("US HOUSE OF REPRESENTATIVES",NC2012House$contest), ]
length(unique(NC2012House$contest))
names(NC2012House)
NC2012House = NC2012House[,-c(3:5,8,10:13,15)]
names(NC2012House)
head(NC2012House)
# County is currently given by a name, I want to give county as a code.
for (i in 1:length(NC2012House)) {
  index = grep(county_name_fips[i,1],NC2012House$county)
  NC2012House$countycode[index] = county_name_fips[i,2]
}
head(NC2012House)
# I now want to combine the county code and the VTD to get the vtd_code
length(unique(NC2012House$vtd))
invalid_codes = setdiff(unique(NC2012House$vtd),unique(NC2010pop$VTD_Code))
for(i in 1:length(invalid_codes)){
  NC2012House = NC2012House[-which(NC2012House$vtd==invalid_codes[i]),]
  
}
setdiff(unique(NC2012House$vtd),unique(NC2010pop$VTD_Code))

length(unique(paste(NC2012House$countycode,NC2012House$vtd,sep="")))
length(unique(NC2010pop$VTD_Key))

# How can I combine this data?
# I need to add a new column to what that gives county codes

