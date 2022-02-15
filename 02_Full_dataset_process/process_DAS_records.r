##### 1. Full data process ##### 

#####Four Full datasets#####
###01.Dchrysippus_DAS_records_Auguest2021_curated_20211124_secondCheck_rm291.csv####Different Format from other three####


###01.process_DAS_records.r####

setwd("/data/martin/genomics/analyses/Danaus_SDM/01_Occurrence/")
file_dir <- "/data/martin/genomics/analyses/Danaus_SDM/01_Occurrence/"
file_prefix <- "Dchrysippus_DAS_records_Auguest2021_curated_20211124_secondCheck_rm291"
records <- read.csv(paste0(file_dir, file_prefix,".csv"), header = T, stringsAsFactors = F)

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
## records$both <- ifelse(records$Sex == "mf", 1, 0)
## records$male <- ifelse(records$Sex == "m", 1, 0)
## records$female <- ifelse(records$Sex == "f", 1, 0)

records$male <- ifelse(records$Sex == "m" | records$Sex == "mf", 1, 0)
records$female <- ifelse(records$Sex == "f" | records$Sex == "mf", 1, 0)

#export processed version
write.csv(records, paste0(file_prefix,".processed.csv"), row.names=F, quote=F)



