# A script to import various data frames about UK electoral map and try to make 
# sense of it.
library(sf)
library(ggplot2)
# Look at the LEAP data - does not have a header.
Leap2018 = read.csv("Data/leap-2018-05-03.csv")
head(Leap2018)
# Look at the larger name
unique(Leap2018[,1])
# Look at the code of larger area
unique(Leap2018[,2])
# Find out how many wards you have data on
length(unique(Leap2018[,4]))

# Look at the boundary commission shape file
BC2023 = st_read("Data/Wards_for_2023_review_SHP/existing_and_prospective_wards_for_2023_review.shp")
dim(BC2023)
test = data.frame(alphabet=c("c","b","a"),number=c(3,2,1))
test
test[order(test$alphabet),]
BC2023 = BC2023[order(BC2023$ons_code),]
BC2023[which(BC2023$ons_code=="E05007562"),]
names(BC2023)
