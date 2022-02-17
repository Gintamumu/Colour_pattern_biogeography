

file_dir <- "/ceph/users/wliu/SP/Danaus_SDM/08_polymorphism/"

file_prefix <- "03_Research_citizen_combine_17094.4DegreeBlocks_rmEnvValue.frequency_processed"

records <- read.csv(paste0(file_dir, file_prefix,".csv"), header = T, stringsAsFactors = F)


records$aa005 <- ifelse(records$hindwingWhite_aa < 0.05, 1, 0)
records$aa005095 <- ifelse(records$hindwingWhite_aa > 0.05 | records$hindwingWhite_aa < 0.95, 1, 0)
records$aa095 <- ifelse(records$hindwingWhite_aa > 0.95, 1, 0)

records$bb005 <- ifelse(records$groundColour_bb < 0.05, 1, 0)
records$bb005095 <- ifelse(records$groundColour_bb > 0.05 | records$groundColour_bb < 0.95, 1, 0)
records$bb095 <- ifelse(records$groundColour_bb > 0.95, 1, 0)

records$cc005 <- ifelse(records$forewingBand_cc < 0.05, 1, 0)
records$cc005095 <- ifelse(records$forewingBand_cc > 0.05 | records$forewingBand_cc < 0.95, 1, 0)
records$cc095 <- ifelse(records$forewingBand_cc > 0.95, 1, 0)

records$any_polymorphic <- ifelse(records$aa005095 == "1" | records$bb005095 == "1" | records$cc005095 == "1", 1, 0)

records$A.005 <- ifelse(1 - records$hindwingWhite_aa < 0.05, 1, 0)
records$A.005095 <- ifelse(1 - records$hindwingWhite_aa > 0.05 | 1 - records$hindwingWhite_aa < 0.95, 1, 0)
records$A.095 <- ifelse(1 - records$hindwingWhite_aa > 0.95, 1, 0)

records$B.005 <- ifelse(1 - records$groundColour_bb < 0.05, 1, 0)
records$B.005095 <- ifelse(1 - records$groundColour_bb > 0.05 | 1 - records$groundColour_bb < 0.95, 1, 0)
records$B.095 <- ifelse(1 - records$groundColour_bb > 0.95, 1, 0) 

records$C.005 <- ifelse(1 - records$forewingBand_cc < 0.05, 1, 0)
records$C.005095 <- ifelse(1 - records$forewingBand_cc > 0.05 | 1 - records$forewingBand_cc < 0.95, 1, 0)
records$C.095 <- ifelse(1 - records$forewingBand_cc > 0.95, 1, 0)

##Area_Identification
library(sp)
library(rworldmap)
# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail

  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  


  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)

  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}


# records$block_midpoint_long
# records$block_midpoint_lat



points = data.frame(lon=records$block_midpoint_long, lat=records$block_midpoint_lat)

records$continent <- coords2continent(points)

# records$country <- coords2country(points)

library(maps)

records$country <- map.where(database="world", records$block_midpoint_long, records$block_midpoint_lat)




#export processed version

write.csv(records, paste0(file_prefix,".polymorphism_processed.csv"), row.names=F, quote=F)










