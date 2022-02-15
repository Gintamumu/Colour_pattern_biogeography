#### 2.Environmental_value_add_and_check ####

####Input files####
##01 Dchrysippus_DAS_records_Auguest2021_curated_20211124_secondCheck_rm291.processed2.csv##
##02 Martin_and_Kenya_collection_20211123_edithead2.processed2.csv##
##03 GBIF_Dchrysippus_20201206_20210721_20211101_2_2_5644_177_research2.processed2.csv##
##04 GBIF_Dchrysippus_20201206_20210721_20211101_2_2_5644_5467_citizen.processed_coordinate_edit1.csv####
####Same codes####

#Load Libraries
library(raster)
library(rgeos)
library(rgdal)
library(maptools) 
data(wrld_simpl)
library(maps)

#setwd workplace
setwd("/ceph/users/wliu/SP/Danaus_SDM/03_value_check/Full/research/")

# variables_check
### Environmental Source https://www.climond.org/BioclimData.aspx
### Annual
Bio01 <- raster("/ceph/users/wliu/SP/Danaus_SDM/02_Layers/asc/01_Bio/CM10_1975H_Bio01_V1.2.asc")
Bio12 <- raster("/ceph/users/wliu/SP/Danaus_SDM/02_Layers/asc/01_Bio/CM10_1975H_Bio12_V1.2.asc")
Bio20 <- raster("/ceph/users/wliu/SP/Danaus_SDM/02_Layers/asc/01_Bio/CM10_1975H_Bio20_V1.2.asc")
Bio28 <- raster("/ceph/users/wliu/SP/Danaus_SDM/02_Layers/asc/01_Bio/CM10_1975H_Bio28_V1.2.asc")

### Seasonality
Bio04 <- raster("/data/martin/genomics/analyses/Danaus_SDM/02_Layers/asc/01_Bio/CM10_1975H_Bio04_V1.2.asc")
Bio15 <- raster("/data/martin/genomics/analyses/Danaus_SDM/02_Layers/asc/01_Bio/CM10_1975H_Bio15_V1.2.asc")
Bio23 <- raster("/data/martin/genomics/analyses/Danaus_SDM/02_Layers/asc/01_Bio/CM10_1975H_Bio23_V1.2.asc")
Bio31 <- raster("/data/martin/genomics/analyses/Danaus_SDM/02_Layers/asc/01_Bio/CM10_1975H_Bio31_V1.2.asc")


# ---------------_checkING DISTRIBUTION DATA FOR NA POINTS
file_dir <- "/ceph/users/wliu/SP/Danaus_SDM/03_value_check/Full/research/"
file_prefix <- "Dchrysippus_DAS_records_Auguest2021_curated_20211124_secondCheck_rm291.processed2.ValueCheck4"
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




