#### 4.Trait/phenotype/genotype frequency and allele frequency calculation #####

#### Trait frequency is applied for GLS/lm/Pearson correlation analysis #######
#### Allele frequency is applied for Cline analysis ###########################
#### Three datasets including research, citizen, and research + citizen combined could be calculated separately for ###
#### Step 8. Residual_Map of Trait frequency between research and citizen datasets.###

setwd("/data/martin/genomics/analyses/Danaus_SDM/03_value_check/4DB/")

file_dir <- "/data/martin/genomics/analyses/Danaus_SDM/03_value_check/4DB/"

file_prefix <- "03_Research_citizen_combine_17094.4DegreeBlocks"

records <- read.csv(paste0(file_dir, file_prefix,".csv"), header = T, stringsAsFactors = F)

#Trait_Frequency
records$hindwingWhite_aa <- round(records$aa/(records$aa+records$A.), 3)
records$forewingBand_cc <- round(records$cc/(records$cc+records$C.), 3)
records$groundColour_bb <- round(records$bb/(records$bb+records$B.), 3)

#Allele_Frequency
records$hindwingWhite_a <- round(sqrt(records$aa/(records$aa+records$A.)), 3)
records$forewingBand_c <- round(sqrt(records$cc/(records$cc+records$C.)), 3)
records$groundColour_b <- round(sqrt(records$bb/(records$bb+records$B.)), 3)

#Sex_Frequency
records$Female_ZW <- round(sqrt(records$female/(records$female+records$male)), 3)
records$Male_ZZ <- round(sqrt(records$male/(records$female+records$male)), 3)

#Number_of_recordings
records$hindwingWhite_n <- round(records$aa+records$A., 3)
records$forewingBand_n <- round(records$cc+records$C., 3)
records$groundColour_n <- round(records$bb+records$B., 3)
records$sex_n  <- round(records$female+records$male, 3)

#### Small frequency map for each trait without world background####### 
idx <- which(is.na(records$hindwingWhite_aa)==FALSE)
plot(records$block_midpoint_long[idx], records$block_midpoint_lat[idx], pch=19,
     col=rgb(records$hindwingWhite_aa[idx],0,0), cex=2)

idx <- which(is.na(records$forewingBand_cc)==FALSE)
plot(records$block_midpoint_long[idx], records$block_midpoint_lat[idx], pch=19,
     col=rgb(records$forewingBand_cc[idx],0,0), cex=2)

idx <- which(is.na(records$groundColour_bb)==FALSE)
plot(records$block_midpoint_long[idx], records$block_midpoint_lat[idx], pch=19,
     col=rgb(records$groundColour_bb[idx],0,0), cex=2)

idx <- which(is.na(records$hindwingWhite_a)==FALSE)
plot(records$block_midpoint_long[idx], records$block_midpoint_lat[idx], pch=19,
     col=rgb(records$hindwingWhite_a[idx],0,0), cex=2)

idx <- which(is.na(records$forewingBand_c)==FALSE)
plot(records$block_midpoint_long[idx], records$block_midpoint_lat[idx], pch=19,
     col=rgb(records$forewingBand_c[idx],0,0), cex=2)

idx <- which(is.na(records$groundColour_b)==FALSE)
plot(records$block_midpoint_long[idx], records$block_midpoint_lat[idx], pch=19,
     col=rgb(records$groundColour_b[idx],0,0), cex=2)

idx <- which(is.na(records$Female_ZW)==FALSE)
plot(records$block_midpoint_long[idx], records$block_midpoint_lat[idx], pch=19,
     col=rgb(records$Female_ZW[idx],0,0), cex=2)

#export processed version

write.csv(records, paste0(file_prefix,".frequency_processed.csv"), row.names=F, quote=F)

### Caution: The file covers all 4 degree block cells from the whole world. It will lead to both NA cells and cells with frequency 
### value shown on whole frequency map, a bad plot, so you have to standardize the file by removing all frequency NA individuals.#####
### Origin file could be saved for step 8. Residual_Map of Trait frequency between research and citizen datasets.####