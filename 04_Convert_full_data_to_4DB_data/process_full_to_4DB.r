
#### 3. Convert_full_data_to_4DB_data #####

#a function that divides the world into blocks of a given size
#it returns the longitude and latitude 'name' of each block and its midpoint coordinates
define_all_blocks <- function(blocksize){
    #use the cut() and levels() functions to divide the world and get the 'name' of each division
    longitude_level_names <- levels(cut(0, breaks = seq(-180, 180, blocksize)))
    latitude_level_names <- levels(cut(0, breaks = seq(-90, 90, blocksize)))
    
    #get the longitude and latitude midpoint of each level
    midpoint_long <- seq(-180+blocksize/2, 180, blocksize)
    midpoint_lat <- seq(-90+blocksize/2, 90, blocksize)
    
    #and add the level "name" for each midpoint value
    names(midpoint_long) <- levels(longitude_level_names)
    names(midpoint_lat) <- levels(latitude_level_names)
    
    #now define ALL bloicks by taking all possible combinations of longitude and latitude
    blocks <- expand.grid(longitude_level_names, latitude_level_names)
    
    #add column names for the blocks
    names(blocks) <- c("block_name_long", "block_name_lat")
    
    #now add the midpoints for each block
    #this is added by retrieving the midpoint value using the block's longitude and latitude names    
    blocks$block_midpoint_long <- midpoint_long[blocks$block_name_long]
    blocks$block_midpoint_lat <- midpoint_lat[blocks$block_name_lat]
    
    blocks
    }

# a function that assigns each record to a block based on its coordinates
# it returns a list of rows giving the records that lie within each block
# many of the blocks will have zero records, because this outputs ALL blocks in the world,
# and many will be in the ocean etc.
group_records_into_blocks <- function(longitude, latitude, blocksize, block_data){
    #use cut() to assign a longitude and latitude level for each record
    block_assignment_long <- cut(longitude, breaks = seq(-180, 180, blocksize))
    block_assignment_lat <- cut(latitude, breaks = seq(-90, 90, blocksize))
    
    #now for each block in the world, retrieve the records that match the correct name for BOTH longitude AND latitude
    lapply(1:nrow(block_data), function(i) which(block_assignment_long == block_data[i,"block_name_long"] &
                                                 block_assignment_lat == block_data[i,"block_name_lat"]))
    }
 


# file_prefix <- "4DB_input"
# file_prefix <- "GBIF_Dchrysippus_20201206_20210721.processed"
file_prefix <- "01_Research_GBIF_DAS_Martin_Kenya_11627_4_coordinate_exchange"

records <- read.csv(paste0(file_prefix,".csv"), header = T, stringsAsFactors = F)

#first we define all possible blocks in the world, and get just their latitude and longitude 'name' and coordinates
block_data <- define_all_blocks(blocksize=4)

#next, for each block, we identify the records that fall within that block
rows_by_block <- group_records_into_blocks(records$decimalLongitude, records$decimalLatitude,
                                           blocksize=4, block_data=block_data)

########################### counts for each block  #############################

#for each trait below, the records file has a count of number of individuals with that trait
# We have already recorded which records fall with in each block, so now we just sum
# the number of individuals with each trait for each block

traits = c("Total", "male", "female", "aa", "A.", "bb", "B.", "cc", "C.")

counts_by_block <- t(sapply(rows_by_block, function(rows) apply(records[rows, traits], 2, sum, na.rm=T)))

#add these counts to the block data frame
block_data <- cbind(block_data, counts_by_block)


#######################  environmental variables   #############################

#for each environmental variable, just take the mean for the block

env_variables <- c("Bio01", "Bio12", "Bio20", "Bio28", "Bio04", "Bio15", "Bio23", "Bio31")

for (env_variables in env_variables){
    env_var_block_means <- sapply(rows_by_block, function(rows) mean(records[rows, env_variables], na.rm=T))
    block_data[,env_variables] <- env_var_block_means
    }

# write.csv(block_data,paste0(file_prefix,".4DegreeBlocks.csv"), row.names=F, quote=F)


write.csv(block_data[,-c(1,2)],paste0(file_prefix,".4DegreeBlocks.csv"), row.names=F, quote=F)