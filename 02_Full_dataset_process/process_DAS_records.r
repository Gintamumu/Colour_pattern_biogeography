##### 1. Full data process ##### 

#####Four Full datasets#####
###01.Dchrysippus_DAS_records_Auguest2021_curated_20211124_secondCheck_rm291.csv
####Different Format from citizen dataset####

###01.process_DAS_records.r####

setwd("/file_location/")
file_dir <- "/file_location/"
file_prefix <- "input_file_prefix"
records <- read.csv(paste0(file_dir, file_prefix,".csv"), header = T, stringsAsFactors = F)

###Identification of D. chrysippus genotypes at the A, B and C loci###
###Hindwing White (A-:absent,Aa:partial,aa:present)
###Ground Colour (B-:present/Dark,Bb:intermediate,bb:absent/light)
###Forewing Tip (C-:absent,Cc:intermediate,cc:present)

#first turn all NA vlalues to zero, so we can add without problems
for (trait in c("aa", "Aa", "A.", "bb", "B.", "cc", "Cc", "C.")){
    records[is.na(records[,trait]),trait] <- 0
    }

# we only care about recessive and dominant genotypes, so we sum the heterozygote and hom dominant for A and C loci
records$A. <- records$A. + records$Aa
records$C. <- records$C. + records$Cc

records$Aa <- 0
records$Cc <- 0

## add separate columns for the number of males and females
records$male <- ifelse(records$Sex == "m" | records$Sex == "mf", 1, 0)
records$female <- ifelse(records$Sex == "f" | records$Sex == "mf", 1, 0)

#export processed version
write.csv(records, paste0(file_prefix,".processed.csv"), row.names=F, quote=F)



