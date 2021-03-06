
file_prefix <- "your_file"

###header=T: 1st line as header
###stringsAsFactors = F: not convert character strings into factors, which may make it easy to do such things as replace values.
butterflies <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)

#Replace blanks and unknowns with NAs
butterflies$sex[which(butterflies$sex == "")] <- NA
butterflies$sex[which(butterflies$sex == "UNKNOWN")] <- NA
butterflies$sex[which(butterflies$sex == "Unknown")] <- NA

butterflies$hindwingWhite[which(butterflies$hindwingWhite == "")] <- NA
butterflies$hindwingWhite[which(butterflies$hindwingWhite == "UNKNOWN")] <- NA

butterflies$forewingTip[which(butterflies$forewingTip == "")] <- NA
butterflies$forewingTip[which(butterflies$forewingTip == "UNKNOWN")] <- NA

butterflies$groundColour[which(butterflies$groundColour == "")] <- NA
butterflies$groundColour[which(butterflies$groundColour == "UNKNOWN")] <- NA

butterflies$month[which(butterflies$month == "")] <- NA
butterflies$month[which(butterflies$month == "UNKNOWN")] <- NA

butterflies$year[which(butterflies$year == "")] <- NA
butterflies$year[which(butterflies$year == "UNKNOWN")] <- NA

###To select data that three traits, sex, month and year information exist.
###To find missing values you check for NA in R using the is.na() function. 
###The subset() function is the easiest way to select variables and observations. 
### |: Element-wise logical OR

### butterflies <- subset(butterflies, is.na(hindwingWhite)==FALSE | is.na(forewingTip)==FALSE | is.na(groundColour)==FALSE | is.na(sex)==FALSE | is.na(month)==FALSE | is.na(year)==FALSE)

### or ignore the last subset() command to see how the numbers change.

##number of males and females, number of individuals per month and number of three traits
#check for numbers and spelling mistakes etc
table(butterflies$sex)

table(butterflies$month)
table(butterflies$year)

table(butterflies$hindwingWhite)
table(butterflies$forewingTip)
table(butterflies$groundColour)

#export subset version
### row.names=F: Prevent row names to be written to file
### quote=F: write a csv from R without quoted values
write.csv(butterflies, paste0(file_prefix,".subset.csv"), row.names=F, quote=F)










