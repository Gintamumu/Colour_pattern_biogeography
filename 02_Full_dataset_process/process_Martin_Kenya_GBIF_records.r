### Full dataset process ####

###02.process_Martin_Kenya_GBIF_records.r####

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






