#### 2.Environmental_value_add_and_check ####

####Input files####
###processed full dataset.csv##
###Title and content###
###gbifID	associatedReferences	references	occurrenceID	individualCount	indNumber	lifeStage	sex	hindwingWhite	forewingTip	groundColour	sideVisible	decimalLatitude	decimalLongitude	coordinateUncertaintyInMeters	eventDate	eventTime	year	month	day	identifier	catalogNumber	countryCode	stateProvince	level0Gid	level0Name	level1Gid	level1Name	level2Gid	level2Name	level3Gid	level3Name	institutionCode	Citizen...Research	No	aa	A.	bb	B.	cc	C.	male	female	Total
###3325960434	NA	https://www.inaturalist.org/observations/86131889	https://www.inaturalist.org/observations/86131889	1		Adult	NA	absent	present	light	upperside	22.2445	114.176449	1	2021-07-08T13:46:00	05:46:00Z	2021	7	8	189270459	86131889	HK	Southern	HKG	Hong Kong	HKG.11_1	Southern					iNaturalist	Citizen	1	0	1	1	0	1	0	NA	NA	1
###3325954372	NA	https://www.inaturalist.org/observations/86037377	https://www.inaturalist.org/observations/86037377	1		Adult	male	absent	present	NA	underside	-33.94398	22.462978	NA	2021-07-07T09:42:00	07:42:00Z	2021	7	7	189084077	86037377	ZA	Western Cape	ZAF	South Africa	ZAF.9_1	Western Cape	ZAF.9.4_1	Eden	ZAF.9.4.2_1	George	iNaturalist	Citizen	2	0	1	NA	NA	1	0	1	0	1
###3325926346	NA	https://www.inaturalist.org/observations/85988456	https://www.inaturalist.org/observations/85988456	1		Adult	male	present	present	light	upperside	10.824091	1.124783	NA	2021-06-29T11:19:07	09:19:07Z	2021	6	29	189027184	85988456	BJ	Atakora	BEN	Benin	BEN.2_1	Atakora	BEN.2.5_1	Mat??ri			iNaturalist	Citizen	3	1	0	1	0	1	0	1	0	1


####Same codes####

#Load Libraries
library(raster)
library(rgeos)
library(rgdal)
library(maptools) 
data(wrld_simpl)
library(maps)

#setwd workplace
setwd("/file_location/")

# variables_check
### Environmental Source https://www.climond.org/BioclimData.aspx
### Annual
Bio01 <- raster("/file_location/CM10_1975H_Bio01_V1.2.asc")
Bio12 <- raster("/file_location/CM10_1975H_Bio12_V1.2.asc")
Bio20 <- raster("/file_location/CM10_1975H_Bio20_V1.2.asc")
Bio28 <- raster("/file_location/CM10_1975H_Bio28_V1.2.asc")

### Seasonality
Bio04 <- raster("/file_location/CM10_1975H_Bio04_V1.2.asc")
Bio15 <- raster("/file_location/CM10_1975H_Bio15_V1.2.asc")
Bio23 <- raster("/file_location/CM10_1975H_Bio23_V1.2.asc")
Bio31 <- raster("/file_location/CM10_1975H_Bio31_V1.2.asc")


# ---------------_checkING DISTRIBUTION DATA FOR NA POINTS
file_dir <- "/file_locations/"
file_prefix <- "your_file"
records <- read.csv(paste0(file_dir, file_prefix,".csv"), header = T, stringsAsFactors = F)

###Order of Coordinate is (longitude,latitude)####
DataPoints1 <- records[c(20:21)] 

records$Bio01 <- extract(Bio01, DataPoints1)
records$Bio12 <- extract(Bio12, DataPoints1)
records$Bio20 <- extract(Bio20, DataPoints1)
records$Bio28 <- extract(Bio28, DataPoints1)

records$Bio04 <- extract(Bio04, DataPoints1)
records$Bio15 <- extract(Bio15, DataPoints1)
records$Bio23 <- extract(Bio23, DataPoints1)
records$Bio31 <- extract(Bio31, DataPoints1)

write.csv(records, paste0(file_prefix,".ValueCheck.csv"), row.names=F, quote=F)

##For cases in which coordinates were out of range of the variable layers (N = 283) we manually applied values of nearby areas using Arcmap##
##Manual of "NA Value check by Acrmap" was shown in another word file##

###After last value check with script, please check again with Arcmap in case of coordinate reverse problem####




