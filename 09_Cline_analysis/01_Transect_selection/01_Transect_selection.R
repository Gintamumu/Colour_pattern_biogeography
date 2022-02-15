##### Transect Selection ####

#Install
# install.packages("ggplot2")
# install.packages("maps")
# install.packages("mapproj")
# install.packages("geosphere")


library(ggplot2)
library(maps)
library(mapproj)
library(geosphere)

cap <- function(x, max) ifelse(x <= max, x, max)

## file_prefix <- "your_file_4DB"

## records_all <- read.csv(paste0(file_prefix,".processed.csv"), header = T, stringsAsFactors = F)

records_4deg <- read.csv("4DB.csv", header = T, stringsAsFactors = F)




# regions to include on map
regions = c("Afghanistan"  , "Angola"  , "Albania"  , "Finland"  , "Andorra"  , "United Arab Emirates"  , "Armenia"  , "American Samoa"  , "Australia"  , "French Southern and Antarctic Lands" , "Austria"  , "Azerbaijan"  , "Burundi"  , "Belgium"  , "Benin"  , "Burkina Faso"  , "Bangladesh"  , "Bulgaria"  , "Bahrain"  , "Bosnia and Herzegovina"  , "Belarus"  , "Brunei"  , "Bhutan"  , "Botswana"  , "Central African Republic"  , "Switzerland"  , "China"  , "Ivory Coast"  , "Cameroon"  , "Democratic Republic of the Congo"  , "Republic of Congo"  , "Cook Islands"  , "Comoros"  , "Cape Verde"  , "Cyprus"  , "Czech Republic"  , "Germany"  , "Djibouti"  , "Denmark"  , "Algeria"  , "Egypt"  , "Eritrea"  , "Canary Islands"  , "Spain"  , "Estonia"  , "Ethiopia"  , "Fiji"  , "Reunion"  , "Mayotte"  , "France"  , "Faroe Islands"  , "Micronesia"  , "Gabon"  , "UK"  , "Georgia"  , "Guernsey"  , "Ghana"  , "Guinea"  , "Gambia"  , "Guinea-Bissau"  , "Equatorial Guinea"  , "Greece"  , "Heard Island"  , "Croatia"  , "Hungary"  , "Indonesia"  , "Isle of Man"  , "India"  , "Cocos Islands"  , "Christmas Island"  , "Chagos Archipelago"  , "Ireland"  , "Iran"  , "Iraq"  , "Iceland"  , "Israel"  , "Italy"  , "San Marino"  , "Jersey"  , "Jordan"  , "Japan"  , "Siachen Glacier"  , "Kazakhstan"  , "Kenya"  , "Kyrgyzstan"  , "Cambodia"  , "Kiribati"  , "South Korea"  , "Kosovo"  , "Kuwait"  , "Laos"  , "Lebanon"  , "Liberia"  , "Libya"  , "Liechtenstein"  , "Sri Lanka"  , "Lesotho"  , "Lithuania"  , "Luxembourg"  , "Latvia"  , "Morocco"  , "Monaco"  , "Moldova"  , "Madagascar"  , "Maldives"  , "Marshall Islands"  , "Macedonia"  , "Mali"  , "Malta"  , "Myanmar"  , "Montenegro"  , "Mongolia"  , "Northern Mariana Islands"  , "Mozambique"  , "Mauritania"  , "Mauritius"  , "Malawi"  , "Malaysia"  , "Namibia"  , "New Caledonia"  , "Niger"  , "Norfolk Island"  , "Nigeria"  , "Niue"  , "Bonaire"  , "Netherlands"  , "Norway"  , "Nepal"  , "Nauru"  , "New Zealand"  , "Oman"  , "Pakistan"  , "Panama"  , "Pitcairn Islands"  , "Philippines"  , "Palau"  , "Papua New Guinea"  , "Poland"  , "North Korea"  , "Madeira Islands"  , "Azores"  , "Portugal"  , "Palestine"  , "French Polynesia"  , "Qatar"  , "Romania"  , "Russia"  , "Rwanda"  , "Western Sahara"  , "Saudi Arabia"  , "Sudan"  , "South Sudan"  , "Senegal"  , "Singapore"  , "South Sandwich Islands"  , "South Georgia"  , "Saint Helena"  , "Ascension Island"  , "Solomon Islands"  , "Sierra Leone"  , "El Salvador"  , "Somalia"  , "Serbia"  , "Slovakia"  , "Slovenia"  , "Sweden"  , "Swaziland"  , "Seychelles"  , "Syria"  , "Turks and Caicos Islands"  , "Chad"  , "Togo"  , "Thailand"  , "Tajikistan"  , "Turkmenistan"  , "Timor-Leste"  , "Tonga"  , "Trinidad"  , "Tobago"  , "Tunisia"  , "Turkey"  , "Taiwan"  , "Tanzania"  , "Uganda"  , "Ukraine"  , "Uzbekistan"  , "Vatican"  , "Vietnam"  , "Vanuatu"  , "Wallis and Futuna"  , "Samoa"  , "Yemen"  , "South Africa"  , "Zambia"  , "Zimbabwe")

map <- map_data("world", regions=regions)

#ggplot map function
plot_map <- ggplot(map, aes(x = long, y = lat, group=group)) +
    geom_polygon(fill="gray95", colour = "gray60") +
    labs(x = "", y = "") +
    theme_bw()

#define butterfly range or subsets of the range
whole_range <- coord_map("mercator", xlim = c(-30,160), ylim = c(-50,50))

### entire distributions

# trait <- 1-records_all$hindwingWhite
# trait <- records_all$forewingBand
# trait <- 1-records_all$groundColour

# rows <- which(is.na(trait) == FALSE)

# plot_map + whole_range +
    # geom_point(data = records_all[rows,], 
              # aes(x = decimalLongitude, y = decimalLatitude, color=trait[rows]),
              # inherit.aes = FALSE) +
    # scale_color_gradient(low = "black", high = "red")
    

### entire distributions blocks

# cols = c("black", "red")
# cols = c("SteelBlue", "LightSteelBlue", "LightSalmon","HotPink","BlueViolet")
# cols = c("Sapphire", "Bluebird", "Lagoon","Lemon","Red-orange")

cols = c("BLUE", "GREEN", "YELLOW","ORANGE","RED")

trait <- 1-records_4deg$hindwingWhite_mean
n <- cap(records_4deg$hindwingWhite_n, 12)
trait_name <- "Hindwing white freq."

plot_map + whole_range +
    geom_tile(data = records_4deg, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_gradientn(colours = cols) + labs(fill = trait_name)


trait <- records_4deg$forewingBand_mean
n <- cap(records_4deg$forewingBand_n, 12)
trait_name <- "Forewing band freq."

plot_map + whole_range +
    geom_tile(data = records_4deg, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_gradientn(colours = cols) + labs(fill = trait_name)


trait <- 1-records_4deg$groundColour_mean
n <- cap(records_4deg$groundColour_n, 12)
trait_name <- "Orange ground colour freq."

plot_map + whole_range +
    geom_tile(data = records_4deg, 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_gradientn(colours = cols) + labs(fill = trait_name)


###############################  transects  ############################

library(geosphere)

#define transects 1 to 5
transects <- as.data.frame(rbind(c(lon_start=-14,   lat_start=16,  lon_end=38,  lat_end=-8), #Central Africa
                                 c(lon_start=25,  lat_start=-25, lon_end=40,  lat_end=0),  #South East Africa
                                 c(lon_start=10,  lat_start=10,   lon_end=20,  lat_end=-30),#South West Africa
                                 c(lon_start=-17, lat_start=10,  lon_end=0, lat_end=43), #North West Africa
                                 c(lon_start=100  , lat_start=4, lon_end=120, lat_end=-12)))#South East Asia


# For each transect, we select which blocks are close enough to the transect.
# To do this, we calculate the perpendicular distance of each block to the transect.
# This is called the cross-track distance or distance to grand circle


# max_dist = 750
max_dist = 450

records_4deg_transects <- list()


for (i in 1:nrow(transects)){
    dist_from_transect <- dist2gc(p1=c(transects[i,"lon_start"], transects[i,"lat_start"]),
                                  p2=c(transects[i,"lon_end"], transects[i,"lat_end"]),
                                  p3=records_4deg[,c("block_midpoint_long","block_midpoint_lat")])/1000

    # and also calculate the position along the transect that the perpendicular line intersects
    # This is the along-track distance
    
    transect_pos <- alongTrackDistance(p1=c(transects[i,"lon_start"], transects[i,"lat_start"]),
                                       p2=c(transects[i,"lon_end"], transects[i,"lat_end"]),
                                       p3=records_4deg[,c("block_midpoint_long","block_midpoint_lat")])/1000
    
    # Identify blocks close to the transect and not beyond the start or end
    transect_blocks <- which(dist_from_transect <= max_dist &
                             records_4deg$block_midpoint_long >= min(c(transects$lon_start[i], c(transects$lon_end[i]))) &
                             records_4deg$block_midpoint_long <= max(c(transects$lon_start[i], c(transects$lon_end[i]))) &
                             records_4deg$block_midpoint_lat >= min(c(transects$lat_start[i], c(transects$lat_end[i]))) &
                             records_4deg$block_midpoint_lat <= max(c(transects$lat_start[i], c(transects$lat_end[i]))))
    
    records_4deg_transects[[i]] <- records_4deg[transect_blocks,]
    
    #add position along transect
    
    records_4deg_transects[[i]]$transect_pos <- transect_pos[transect_blocks]
    
    }


#export cline data for analysis in another script.
# I was unable to successfully install hzar in the same conda environment as the R mapping libraries
# It appears that this is because it needed Rcpp 1.0.3 and only 1.0.1 was available on conda

for (i in 1:nrow(transects)){
    write.csv(records_4deg_transects[[i]], file=paste0(file_prefix, ".processed.4DegreeBlocks.transect", i, ".csv"),
              row.names=FALSE, quote=FALSE)
    }


########################  plotting transects  ############################


# First define map regions for the different transects

Central_Africa <- coord_map("mercator", xlim = c(-20,60), ylim = c(-20,30))

Southern_Africa <- coord_map("mercator", xlim = c(5,42), ylim = c(-35,10))

Northwest_Africa <- coord_map("mercator", xlim = c(-20,10), ylim = c(5,45))

Southeast_Asia <- coord_map("mercator", xlim = c(90,130), ylim = c(-15,10))


transect_col = "blue"

# Transect 1 is central Africa and involves hindwing and forewing

i=1

trait <- 1-records_4deg_transects[[i]]$hindwingWhite_mean
n <- cap(records_4deg_transects[[i]]$hindwingWhite_n, 12)
trait_name <- "Hindwing white freq."

plot_map + Central_Africa +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_gradientn(colours = cols) + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2)

trait <- records_4deg_transects[[i]]$forewingBand_mean
n <- cap(records_4deg_transects[[i]]$forewingBand_n, 12)
trait_name <- "Forewing tip freq."

plot_map + Central_Africa +
    geom_tile(data = records_4deg_transects[[i]], 
              aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    scale_fill_gradientn(colours = cols) + labs(fill = trait_name) +
    
    annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2)


# Transect 2 is Southeast Africa and involves forewing and ground colour

# i=2

# trait <- records_4deg_transects[[i]]$forewingBand_mean
# n <- cap(records_4deg_transects[[i]]$forewingBand_n, 12)
# trait_name <- "Forewing tip freq."

# plot_map + Southern_Africa +
    # geom_tile(data = records_4deg_transects[[i]], 
              # aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    # scale_fill_gradientn(colours = cols) + labs(fill = trait_name) +
    
    # annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             # y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2)

# trait <- 1-records_4deg_transects[[i]]$groundColour_mean
# n <- cap(records_4deg_transects[[i]]$groundColour_n, 12)
# trait_name <- "Orange ground colour freq."

# plot_map + Southern_Africa +
    # geom_tile(data = records_4deg_transects[[i]], 
              # aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    # scale_fill_gradientn(colours = cols) + labs(fill = trait_name) +
    
    # annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             # y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2)


# Transect 3 is Southeast Africa and involves hindwing and ground colour

# i=3

# trait <- 1-records_4deg_transects[[i]]$hindwingWhite_mean
# n <- cap(records_4deg_transects[[i]]$hindwingWhite_n, 12)
# trait_name <- "Hindwing white freq."

# plot_map + Southern_Africa +
    # geom_tile(data = records_4deg_transects[[i]], 
              # aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    # scale_fill_gradientn(colours = cols) + labs(fill = trait_name) +
    
    # annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             # y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2)

# trait <- 1-records_4deg_transects[[i]]$groundColour_mean
# n <- cap(records_4deg_transects[[i]]$groundColour_n, 12)
# trait_name <- "Orange ground colour freq."

# plot_map + Southern_Africa +
    # geom_tile(data = records_4deg_transects[[i]], 
              # aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    # scale_fill_gradientn(colours = cols) + labs(fill = trait_name) +
    
    # annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             # y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2)


# Transect 4 is Northwest Africa and involves hindwing and ground colour

# i=4

# trait <- 1-records_4deg_transects[[i]]$hindwingWhite_mean
# n <- cap(records_4deg_transects[[i]]$hindwingWhite_n, 12)
# trait_name <- "Hindwing white freq."

# plot_map + Northwest_Africa +
    # geom_tile(data = records_4deg_transects[[i]], 
              # aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    # scale_fill_gradientn(colours = cols) + labs(fill = trait_name) +
    
    # annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             # y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2)

# trait <- 1-records_4deg_transects[[i]]$groundColour_mean
# n <- cap(records_4deg_transects[[i]]$groundColour_n, 12)
# trait_name <- "Orange ground colour freq."

# plot_map + Northwest_Africa +
    # geom_tile(data = records_4deg_transects[[i]], 
              # aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    # scale_fill_gradientn(colours = cols) + labs(fill = trait_name) +
    
    # annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             # y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2)


# Transect 5 is Southeast Asia and involves hindwing and ground colour

# i=5

# trait <- 1-records_4deg_transects[[i]]$hindwingWhite_mean
# n <- cap(records_4deg_transects[[i]]$hindwingWhite_n, 12)
# trait_name <- "Hindwing white freq."

# plot_map + Southeast_Asia +
    # geom_tile(data = records_4deg_transects[[i]], 
              # aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    # scale_fill_gradientn(colours = cols) + labs(fill = trait_name) +
    
    # annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             # y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2)

# trait <- 1-records_4deg_transects[[i]]$groundColour_mean
# n <- cap(records_4deg_transects[[i]]$groundColour_n, 12)
# trait_name <- "Orange ground colour freq."

# plot_map + Southeast_Asia +
    # geom_tile(data = records_4deg_transects[[i]], 
              # aes(x = block_midpoint_long, y = block_midpoint_lat, fill = trait), inherit.aes = FALSE) +
    # scale_fill_gradientn(colours = cols) + labs(fill = trait_name) +
    
    # annotate(geom="segment", x=transects$lon_start[i], xend=transects$lon_end[i],
             # y=transects$lat_start[i], yend=transects$lat_end[i], colour = transect_col, size=2)



