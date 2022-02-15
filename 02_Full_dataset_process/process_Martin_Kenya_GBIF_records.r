### Full dataset process ####

###02.process_Martin_Kenya_GBIF_records.r####

###Identification of D. chrysippus genotypes at the A, B and C loci###
###Hindwing White (A-:absent,Aa:partial,aa:present)
###Ground Colour (B-:present/Dark,Bb:intermediate,bb:absent/light)
###Forewing Tip (C-:absent,Cc:intermediate,cc:present)

### Input file example ###
### Title and content ###
### gbifID	associatedReferences	references	occurrenceID	individualCount	indNumber	lifeStage	sex	hindwingWhite	forewingTip	groundColour	sideVisible	decimalLatitude	decimalLongitude	coordinateUncertaintyInMeters	eventDate	eventTime	year	month	day	identifier	catalogNumber	countryCode	stateProvince	level0Gid	level0Name	level1Gid	level1Name	level2Gid	level2Name	level3Gid	level3Name	institutionCode	Citizen & Research	No
### 3325960434		https://www.inaturalist.org/observations/86131889	https://www.inaturalist.org/observations/86131889	1		Adult	NA	absent	present	light	upperside	22.2445	114.176449	1	2021-07-08T13:46:00	05:46:00Z	2021	7	8	189270459	86131889	HK	Southern	HKG	Hong Kong	HKG.11_1	Southern					iNaturalist	Citizen	1
### 3325954372		https://www.inaturalist.org/observations/86037377	https://www.inaturalist.org/observations/86037377	1		Adult	male	absent	present	NA	underside	-33.94398	22.462978		2021-07-07T09:42:00	07:42:00Z	2021	7	7	189084077	86037377	ZA	Western Cape	ZAF	South Africa	ZAF.9_1	Western Cape	ZAF.9.4_1	Eden	ZAF.9.4.2_1	George	iNaturalist	Citizen	2

####Three datasets with same codes####
###02.Martin_and_Kenya_collection_20211123_edithead2.csv####The head of this file should be same with GBIF spreadsheet####
###03.GBIF_Dchrysippus_20201206_20210721_20211101_2_2_5644_177_research2.csv####
###04.GBIF_Dchrysippus_20201206_20210721_20211101_2_2_5644_5467_citizen.csv####

file_prefix <- "Martin_and_Kenya_collection_20211123_edithead2"

records <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)

#Replace blanks and unknowns with NAs
records$hindwingWhite[which(records$hindwingWhite == "")] <- NA
records$hindwingWhite[which(records$hindwingWhite == "UNKNOWN")] <- NA

records$forewingTip[which(records$forewingTip == "")] <- NA
records$forewingTip[which(records$forewingTip == "UNKNOWN")] <- NA

records$groundColour[which(records$groundColour == "")] <- NA
records$groundColour[which(records$groundColour == "UNKNOWN")] <- NA

#remove rows with no data at all
records <- subset(records, is.na(hindwingWhite)==FALSE | is.na(forewingTip)==FALSE | is.na(groundColour)==FALSE)

#check for spelling mistakes etc
table(records$sex)
table(records$hindwingWhite)
table(records$forewingTip)
table(records$groundColour)

#convert to numeric
records$aa <- ifelse(records$hindwingWhite == "present", 1, 0)
records$A. <- ifelse(records$hindwingWhite == "absent" | records$hindwingWhite == "partial", 1, 0)

records$bb <- ifelse(records$groundColour == "light", 1, 0)
records$B. <- ifelse(records$groundColour == "dark" | records$groundColour == "intermediate", 1, 0)

records$cc <- ifelse(records$forewingTip == "present", 1, 0)
records$C. <- ifelse(records$forewingTip == "absent" | records$forewingTip == "partial", 1, 0)

records$male <- ifelse(records$sex == "male", 1, 0)
records$female <- ifelse(records$sex == "female", 1, 0)

records$Total <- 1 # the total is always one for individual records. We include this because for other data types it can be > 1

#export processed version

write.csv(records, paste0(file_prefix,".processed.csv"), row.names=F, quote=F)






