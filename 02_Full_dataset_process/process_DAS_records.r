##### Full museum data process ##### 

###01.process_DAS_records.r####

### Input file example 
### Site	Sex	Total 	A-	Aa	aa	C-	Cc	cc	B-	bb	latitude	longitude	decimalLatitude	decimalLongitude	collector	dataset
### 1	m	23	23	0	0	0	0	23	0	23	6.06N	100.23E	6.1	100.383	dass	Smith field notes
### 1	f	20	20	0	0	0	0	20	0	20	6.06N	100.23E	6.1	100.383	dass	Smith field notes



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



